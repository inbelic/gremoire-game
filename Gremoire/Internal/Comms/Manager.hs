module Internal.Comms.Manager
  ( GameTree
  , empty
  , lookup
  , createAndAddConn
  , rmvConn
  , GameID (..)
  , strip
  , dress
  ) where

import Prelude hiding (lookup)

import Internal.Comms.Comms (Conn, newConn)
import Data.Binary (encode, decode)
import Data.Word (Word32)
import qualified Data.ByteString as B
  ( ByteString, append
  , length, splitAt
  , fromStrict, toStrict
  )
import qualified Data.Map.Strict as Map
  ( Map, empty, lookup, insert, delete, keys)

newtype GameTree = GameTree
  { gameTree :: Map.Map GameID Conn
  }

instance Show GameTree where
  show = show . Map.keys . gameTree

-- Some wrapping to avoid the need for external module to import
-- Data.Map
empty :: GameTree
empty = GameTree Map.empty

lookup :: GameID -> GameTree -> Maybe Conn
lookup gID = Map.lookup gID . gameTree

createAndAddConn :: GameID -> GameTree -> IO (GameTree, Conn)
createAndAddConn gID (GameTree gameTree) = do
    conn <- newConn
    return (GameTree $ Map.insert gID conn gameTree, conn)

rmvConn :: GameID -> GameTree -> GameTree
rmvConn gID = GameTree . Map.delete gID . gameTree

-- Keep track of which game we want to respond/interact with
newtype GameID = GameID
  { gameID :: Word32
  }
  deriving (Eq, Ord, Show)

strip :: B.ByteString -> Maybe (GameID, B.ByteString)
strip bytes
  | len < 4 = Nothing
  | otherwise = Just (GameID . decode . B.fromStrict $ gIDBytes, otherBytes)
  where
    len = B.length bytes
    (gIDBytes, otherBytes) = B.splitAt 4 bytes

dress :: GameID -> B.ByteString -> B.ByteString
dress (GameID gID) = B.append gIDBytes
  where
    gIDBytes = B.toStrict $ encode gID
