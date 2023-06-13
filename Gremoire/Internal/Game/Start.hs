module Internal.Game.Start
  ( invokeGame
  ) where

-- This module encapsulates all things required to start up a new
-- game on the server

import Internal.Game.Load (LoadInfo, loadGame)
import Internal.Comms.Comms (Conn, gameWrite, gameRead)
import Internal.Game.Engine (resolveStack)
import Internal.Game.Types (Game)

import Control.Monad (void)

import qualified Data.ByteString as B (concat, cons)

runGame :: Conn -> LoadInfo -> Game -> IO ()
runGame conn loadInfo game = void $ resolveStack loadInfo conn game

invokeGame :: Conn -> IO ()
invokeGame conn = do
    startInfo <- gameRead conn
    (loadInfo, game) <- loadGame startInfo
    runGame conn loadInfo game
