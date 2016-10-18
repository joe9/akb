{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe
import qualified Data.Text.IO as TIO

import                  Network.NineP.Server
import BitMask
--
import Akb
import Modifiers
import State
import KeySymbolDefinitions

main :: IO ()
main = do
  TIO.putStrLn
    (showKeySymbol ((getKeySymbol (pickInitialState 1) 10)))
  print (fromBitMask 0 :: [Modifier])
  print (fromBitMask 1 :: [Modifier])
  print (fromBitMask 2 :: [Modifier])
  print (fromBitMask 3 :: [Modifier])
  print (fromBitMask 4 :: [Modifier])
  print (fromBitMask 5 :: [Modifier])
  print (fromBitMask 6 :: [Modifier])
  print (fromBitMask 7 :: [Modifier])
  print (fromBitMask 255 :: [Modifier])
  print (fromBitMask 0 :: Maybe Modifier)
  print (fromBitMask 1 :: Maybe Modifier)
  print (fromBitMask 2 :: Maybe Modifier)
  print (fromBitMask 4 :: Maybe Modifier)
  context <- initializeContext
  run9PServer context

getKeySymbol :: State -> KeyCode -> KeySymbol
getKeySymbol s k = lookupKeyCode k s

