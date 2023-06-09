module Core.Callbacks
  ( displayState
  , numberPlayers
  , orderPlayers
  ) where

import Core.Fields
import Core.CardState
import Internal.Bytes
import Internal.Misc
import Internal.Game.Load (LoadInfo, compiledMasks)
import Internal.Game.Types
import Internal.Game.Views
import qualified Data.ByteString as B -- (ByteString)
import qualified Data.Map as Map (Map, foldrWithKey, lookup, intersectionWith)

-- Here we can define any custom ways of how to display the current game
-- state that will be sent to the players
--
-- The outputted ByteString will be parsed by the external server side code
-- and will send the ByteString segments to the player clients follow the
-- simple specification. If there are N players then for each player, the
-- first byte of their recv bytestring with denote how many bytes are theirs.
-- We then send those bytes to player 0 and do the same until we reach the
-- end of the bytes. As an example, let N = 3 and N_i denote which player
-- the bytes go to. So N_0 = 0, N_1 = 5, N_2 = 3. Then we would send a message
-- of N_0 + N_1 + N_2 + N = 0 + 5 + 3 + 3 = 11 and it would look like:
-- N_0:N_1:1:2:3:4:5:N_2:6:7:8. Where the numbers denote a byte and each of
-- N_i < 255.

displayState :: LoadInfo -> GameState -> B.ByteString
displayState loadInfo (GameState _ _ cardState abilityState)
  = B.cons 0 -- Extra zero to provide to the game_system
  . B.concat . map tagSize . zipWith (displayCards masks) [1..numberPlayers]
  . repeat . Map.intersectionWith (,) cardState
  $ abilityState
  where
    masks = getMasks . compiledMasks $ loadInfo

    displayCards :: (Masks, Masks, Masks) -> Int
                    -> Map.Map CardID (FieldMap, StmtMap) -> B.ByteString
    displayCards masks ownerInt
      = tagPlayerID ownerInt . Map.foldrWithKey (displayCard masks owner) B.empty
      where
        owner = enumToU8 ownerInt

    displayCard :: (Masks, Masks, Masks) -> Owner ->
                   CardID -> (FieldMap, StmtMap) -> B.ByteString -> B.ByteString
    displayCard (sysMask, allyMask, opMask) owner (CardID cID) (fm, stmtIDs)
      | showCID = B.cons (u8ToEnum cID) . B.append fmBytes . B.append stmtBytes
      | otherwise = B.cons 0 . B.append fmBytes . B.cons 0
        where
          mask = case check' Owner (U8 0) fm of
                   (U8 0) -> sysMask
                   x -> if owner == x
                           then allyMask
                           else opMask
          fm' = filterMasks mask fm
          fmBytes = displayFieldMap fm'
          stmtBytes = displayStmtMap stmtIDs
          showCID = check' CardNum (U8 0) fm' /= U8 0
          cardBytes
            = B.cons (u8ToEnum cID)
            $ B.append (displayFieldMap fm) (displayStmtMap stmtIDs)

    displayStmtMap :: StmtMap -> B.ByteString
    displayStmtMap = tagSize . Map.foldrWithKey displayStmt B.empty

    displayStmt :: AbilityID -> StatementID -> B.ByteString -> B.ByteString
    displayStmt (AbilityID aID) (StatementID sID)
      = B.cons (u8ToEnum aID)
      . B.cons (u8ToEnum sID)

    displayFieldMap :: FieldMap -> B.ByteString
    displayFieldMap fm = tagSize $ Map.foldrWithKey displayValue B.empty fm

    displayValue :: Field -> U8 -> B.ByteString -> B.ByteString
    displayValue field val
      = B.cons (toEnum . fromEnum $ field)
      . B.cons (u8ToEnum val)

    tagPlayerID :: Int -> B.ByteString -> B.ByteString
    tagPlayerID = B.cons . toEnum

-- We also provide a callback to determine which players triggers should be
-- place on the stack first based on the current game state. If we have
-- N players that are playing then we will return an N length list that
-- is a permutation of 1..N

numberPlayers :: Int
numberPlayers = gremoireNumberPlayers

orderPlayers :: GameState -> [Owner]
orderPlayers = gremoireOrderPlayers







-- In gremoire, we will determine the order of players based on who is currently
-- on the offense. Whoever dealt the most recent damage in the last
-- skirmish is considered to be on the offense, tie goes to the previous round
-- winner, player 1 defaults if there has been no damage dealt yet. As such
-- their triggers will be ordered last so that they will resolve first. All
-- system triggers will be placed in the stack first so that they resolve last.

-- A player damaging another player during the skirmish phase is denoted by
-- finding an Shift Alteration between the two Hero cards during
-- the Skirmish phase
gremoireNumberPlayers :: Int
gremoireNumberPlayers = 2

-- TODO:
gremoireOrderPlayers :: GameState -> [Owner]
gremoireOrderPlayers _ = [2, 1, 0]
