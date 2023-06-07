module Internal.Comms.Cmds
  ( Cmd(..)
  , display
  , order
  , target
  , result
  ) where

-- Bindings that correspond to slatch/include/cmds.hrl and the client side.
-- We will only export the cmds that are used internally but will list all of
-- them for completeness

import Data.Word (Word8)
import Data.Char (chr)

-- WARNING: Ensure that the erlang server is updated if you changes these

-- General server cmds
newtype Cmd = Cmd
  { getCmd :: Word8
  }

ok :: Cmd 
ok = mkCmd 0

invalid :: Cmd
invalid = mkCmd 1

echo :: Cmd
echo = mkCmd 2

-- Lobby (Unused but kept for clarity of enum values)

login :: Cmd
login = mkCmd 3

queue :: Cmd
queue = mkCmd 4

started :: Cmd
started = mkCmd 5

-- Playing (client cmds)
display :: Cmd
display = mkCmd 6

order :: Cmd
order = mkCmd 7

target :: Cmd
target = mkCmd 8

result :: Cmd
result = mkCmd 9

-- Helper to convert
mkCmd :: Int -> Cmd
mkCmd = Cmd . toEnum
