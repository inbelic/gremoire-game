module Main where

import Internal.Comms.Harness (tcpHarness)
import System.Environment (getArgs)

import Core.Fields
import Core.Card

main :: IO ()
main = tcpHarness . head =<< getArgs
