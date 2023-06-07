module Main where

import Internal.Comms.Harness (tcpHarness)
import System.Environment (getArgs)

import Core.Fields
import Core.Views
import Core.Card
import Internal.Game.Cut

main :: IO ()
main = tcpHarness . head =<< getArgs
