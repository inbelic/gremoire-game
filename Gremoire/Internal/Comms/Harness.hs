module Internal.Comms.Harness
  ( tcpHarness
  ) where

-- This module defines the various networking components to start our
-- application and connect with the erlang harness side

import qualified Control.Exception as E (bracket, bracketOnError)
import Control.Concurrent (forkIO)

import Internal.Comms.Comms (newConn, harnessWrite, harnessRead)
import Internal.Comms.Porter (portLogStr, portLog)
import qualified Internal.Comms.Manager as M
import Internal.Game.Start (invokeGame)

import qualified Data.ByteString as B (ByteString, uncons)
import Network.Socket
  ( Socket, AddrInfo, HostName, ServiceName, SocketType(..)
  , defaultHints, addrSocketType, getAddrInfo, addrAddress
  , withSocketsDo, openSocket, connect, close
  )
import Network.Socket.ByteString (recv, sendAll)

localHost :: HostName
localHost = "127.0.0.1"

tcpHarness :: String -> IO ()
tcpHarness port = do
    portLogStr $ "connecting on localhost:" ++ port ++ "..."
    runTCPClient localHost port (harnessLoop M.empty)

harnessLoop :: M.GameTree -> Socket -> IO ()
harnessLoop gameTree sock = do
    srvrResponse <- recv sock 256
    case (=<<) M.strip . fmap snd . B.uncons $ srvrResponse of
      Nothing -> do
        portLogStr "exiting connection..."
      (Just (gID, response)) ->
        handleRequest gameTree sock gID response

handleRequest :: M.GameTree -> Socket -> M.GameID -> B.ByteString -> IO ()
handleRequest gameTree sock gID response
  = case M.lookup gID gameTree of
      Nothing -> do
        (gameTree', conn) <- M.createAndAddConn gID gameTree
        portLogStr $ "creating new game: " ++ show gID
        forkIO (invokeGame conn)
        handleRequest gameTree' sock gID response
      (Just conn) -> do
        harnessWrite conn response
        nxtGameRequest <- M.dress gID <$> harnessRead conn
        sendAll sock nxtGameRequest
        harnessLoop gameTree sock

-- following from Network.Socket example
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
    where
      resolve :: IO AddrInfo
      resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)

      open :: AddrInfo -> IO Socket
      open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
