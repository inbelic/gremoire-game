module Core.Callbacks
  ( displayState
  , numberPlayers
  , orderPlayers
  ) where

import Core.Fields -- (Owner)
import Internal.Bytes
import Internal.Misc
import Internal.Game.Load (LoadInfo)
import Internal.Game.Types
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
displayState _ (GameState _ _ cardState abilityState)
  = B.cons 0 . B.concat . replicate numberPlayers
  . tagSize . Map.foldrWithKey displayCard B.empty
  . Map.intersectionWith (,) cardState
  $ abilityState
  where
    displayCard :: CardID -> (FieldMap, StmtMap) -> B.ByteString
                -> B.ByteString
    displayCard (CardID cID) (fm, stmtIDs) = B.append cardBytes
        where
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

gremoireOrderPlayers :: GameState -> [Owner]
gremoireOrderPlayers _ = [0, 1, 2]
