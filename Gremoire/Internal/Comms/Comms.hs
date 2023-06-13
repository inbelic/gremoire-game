module Internal.Comms.Comms
  -- Various Conn exports
  ( Conn, newConn
  , harnessWrite, harnessRead
  , gameWrite, gameRead
  -- Request exports
  , Comm, displayState
  , requestOrder, requestTargets
  ) where

import Core.Fields (Owner)
import Control.Monad (when, zipWithM)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import qualified Data.ByteString as B
import Internal.Bytes
import Internal.Comms.Cmds
import qualified Core.Callbacks as Callback
import Internal.Game.Load (LoadInfo(..))
import Internal.Game.Types
import Internal.Misc (reorder, tagSize, maybeHead)

newtype Conn = Conn
  { getChans :: (Chan B.ByteString, Chan B.ByteString)
  }

newConn :: IO Conn
newConn = do
  gameCh <- newChan
  harnessCh <- newChan
  return $ Conn (gameCh, harnessCh)

-- Define functions to extract the channel to prvent accidently reading from
-- the wrong channel and causing a deadlock
--
-- harness* should only be invoked from the Harness module
harnessWrite :: Conn -> B.ByteString -> IO ()
harnessWrite = writeChan . snd . getChans

harnessRead :: Conn -> IO B.ByteString
harnessRead = readChan . fst . getChans

-- game* should only be invoked from this module or Game.Start.hs
gameWrite :: Conn -> B.ByteString -> IO ()
gameWrite = writeChan . fst . getChans

gameRead :: Conn -> IO B.ByteString
gameRead = readChan . snd . getChans

-- Here we define the various requests that the server will need to resolve
-- by communicating with the outside world
type Comm a = Conn -> a -> IO a

-- Here we wrap the callback function by taggin it with the size and ensuring
-- that all clients have responded with an okay
displayState :: LoadInfo -> GameState -> Comm ()
displayState loadInfo gameState conn () = do
    gameWrite conn . B.cons (getCmd display)
                   . Callback.displayState loadInfo $ gameState
    response <- gameRead conn
    when (response /= emp) $ displayState loadInfo gameState conn ()
      where
        emp = B.cons 0 . B.cons 0 . B.cons 0 $ B.empty

requestOrder :: LoadInfo -> GameState -> Comm [Header]
requestOrder _ _ _ [] = return []
requestOrder loadInfo gameState conn hdrs = do
    displayState loadInfo gameState conn ()
    let groupedHdrs = map snd
                    . flip (foldr groupHeaders) hdrs
                    $ fillHeaders outputOrder
        fmtHdrs = map (foldr fmtHeader B.empty) groupedHdrs
        numPlyrs = Callback.numberPlayers
        outputOrder = [0.. numPlyrs]
    gameWrite conn . B.cons (getCmd order) . B.concat . map tagSize $ fmtHdrs
    response <- gameRead conn
    let responses = reverse . map toOrder $ groupResponses response []
        orders = map fromEnum $ Callback.orderPlayers gameState
        orderedHdrs = fmap concat
                    . (=<<) (`reorder` map (+ 1) orders)
                    $ zipWithM reorder groupedHdrs responses
    case orderedHdrs of
        Nothing ->
            requestOrder loadInfo gameState conn hdrs
        (Just hdrs) -> return hdrs

    where
      fmtHeader :: Header -> B.ByteString -> B.ByteString
      fmtHeader (Unassigned _ (CardID cID) (AbilityID aID))
        = B.cons (u8ToEnum cID) . B.cons (u8ToEnum aID)
      fmtHeader (Assigned _ (CardID cID) (AbilityID aID) _)
        = B.cons (u8ToEnum cID) . B.cons (u8ToEnum aID)

      -- Our responses then are all u8 integers of the orderings
      toOrder :: B.ByteString -> [Int]
      toOrder = B.foldr f []
        where
          f byte = (fromEnum byte :)

requestTargets :: Comm Header
requestTargets conn hdr@Unassigned{} = return hdr
requestTargets conn hdr@(Assigned owner cID aID targets)
  = fmap (Targeted owner cID aID)
  . mapM (requestTarget conn hdr)
  $ targets
    where
      requestTarget :: Conn -> Header -> (TargetID, Target)
                    -> IO (TargetID, Create CardID)
      requestTarget conn hdr (tID, target)
        = case target of
            Void -> return (tID, Create)
            (Given cID) -> return (tID, Existing cID)
            (Inquire range) -> doRequest hdr tID range conn
            (Random range) -> doRequest (changeOwnerToSystem hdr) tID range conn

      -- If our target is random then the system will need to target for us
      changeOwnerToSystem :: Header -> Header
      changeOwnerToSystem (Assigned _ cID aID targets)
        = Assigned (U8 0) cID aID targets

      doRequest :: Header -> TargetID -> Range -> Conn
                -> IO (TargetID, Create CardID)
      doRequest hdr@(Assigned owner cID aID _) tID range conn = do
        let wrappedHdrs = map snd . groupHeaders hdr $ fillHeaders outputOrder
            outputOrder = [0.. numPlyrs]
            numPlyrs = Callback.numberPlayers
        gameWrite conn . B.cons (getCmd target) . B.concat
                       . map (tagSize . fmtTarget cID aID range)
                       $ wrappedHdrs
        response <- gameRead conn
        let tcID -- extract out the byte that contains the target
                 = fmap (enumToU8 . fst)
                 . (=<<) B.uncons . maybeHead
                 -- Strip away the responses of those who didn't target
                 . drop (fromEnum owner) . reverse
                 $ groupResponses response []
        case tcID of
          -- Wasn't targeted by the owner so redo
          Nothing -> doRequest hdr tID range conn
          (Just tcID) ->
            -- Ensure the target was in the provided range
            if elem tcID . map cardID $ range
               then return (tID, Existing . CardID . enumToU8 $ tcID)
               else doRequest hdr tID range conn

      fmtTarget :: CardID -> AbilityID -> Range -> [Header] -> B.ByteString
      fmtTarget (CardID x) (AbilityID y) bytes hdrs
        | null hdrs = B.empty
        | otherwise
            = B.cons (u8ToEnum x) . B.cons (u8ToEnum y)
            . foldr (B.cons . u8ToEnum . cardID) B.empty $ bytes

-- The output will expect some sort of output for every user with the
-- given format. Hence, we fill the owners that don't require any input
-- with empty lists.
fillHeaders :: [Int] -> [(Owner, [Header])]
fillHeaders = map ((,[]) . enumToU8)

-- So we can route all the owners headers together into one request
-- we will group them here
groupHeaders :: Header -> [(Owner, [Header])] -> [(Owner, [Header])]
groupHeaders hdr@(Unassigned owner _ _) = map (ownerMap owner hdr)
groupHeaders hdr@(Assigned owner _ _ _) = map (ownerMap owner hdr)

ownerMap :: Owner -> Header -> (Owner, [Header]) -> (Owner, [Header])
ownerMap owner hdr (otherOwner, hdrs)
  | owner == otherOwner = (owner, hdr : hdrs)
  | otherwise = (otherOwner, hdrs)

-- We will got through our bytestring and split the requests into their
-- appropriate users
groupResponses :: B.ByteString -> [B.ByteString] -> [B.ByteString]
groupResponses bytes
  = case B.uncons bytes of
      Nothing -> id
      (Just (size, bytes')) ->
        let (cur, rest) = B.splitAt (fromEnum size) bytes'
         in groupResponses rest . (cur :)
