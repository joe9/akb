{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe
import qualified Data.Text.IO                     as TIO
import qualified Data.Vector                      as V
import           GHC.Base
import           GHC.Show
import           Protolude                        hiding (State, show)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Text.Groom

import           BitMask
import           Network.NineP.Context hiding (File)
import qualified Network.NineP.Context as Context
import           Network.NineP.Server

import Akb
import KeySymbolDefinitions
import Modifiers
import State

main :: IO ()
main = do
  TIO.putStrLn (showKeySymbol ((getKeySymbol (pickInitialState 1) 10)))
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
  let context = initializeContext {cFSItems = fsList}
  run9PServer context

getKeySymbol :: State -> KeyCode -> KeySymbol
getKeySymbol s k = lookupKeyCode k s

fsList :: V.Vector (FSItem Context)
fsList =
  V.fromList
    [ dir "/"
    , file "/in"
    , file "/out"
    , dir "/modifiers/"
    , dir "/modifiers/effective"
    , file "/modifiers/effective/out"
    , dir "/modifiers/depressed"
    , file "/modifiers/depressed/out"
    , dir "/modifiers/latched"
    , file "/modifiers/latched/out"
    , dir "/modifiers/locked"
    , file "/modifiers/locked/out"
    , dir "/group/"
    , dir "/group/effective"
    , file "/group/effective/out"
    , dir "/group/depressed"
    , file "/group/depressed/out"
    , dir "/group/latched"
    , file "/group/latched/out"
    , dir "/group/locked"
--     , FSItem Context.File
--        ((fileDetails  "/group/locked/out") { dRead = }) []
    ]

dir, file :: RawFilePath -> FSItem Context
dir name = FSItem Directory (dirDetails name) []
file name = FSItem Context.File (fileDetails name) []
