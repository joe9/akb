{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe
import qualified Data.ByteString                  as BS
import qualified Data.Text.IO                     as TIO
import qualified Data.Vector                      as V
import Data.String.Conversions
import           GHC.Show
import           Protolude                        hiding (State, show)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Text.Groom
import           Data.Attoparsec.ByteString.Char8
import           Data.Word

import           BitMask
import           Network.NineP.Context hiding (File)
import qualified Network.NineP.Context as Context
import           Network.NineP.Server
import Network.NineP.Error
import           Data.NineP hiding (Directory)

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
  case parseOnly inputParser "10,Pressed" of
    Left e -> print e
    Right (Input kcode dir) -> print (keyEvent (pickInitialState 1) kcode dir)
--   let context = initializeContext {cFSItems = fsList}
--   run9PServer context

getKeySymbol :: State -> KeyCode -> KeySymbol
getKeySymbol s k = lookupKeyCode k s

fsList :: V.Vector (FSItem Context)
fsList =
  V.fromList
    [ dir "/"
    , file "/in" -- 1
    , file "/out" -- 2
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

inFileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> s
  -> IO (Either NineError Count, s)
inFileWrite fid offset bs fidState me c = do
  case parseOnly inputParser bs of
    Left e -> return ((Left . OtherError . cs) e, c)
    Right (Input kcode dir) -> do
--         (keyEvent (pickInitialState 1) kcode dir)
        return ((Right . fromIntegral . BS.length) bs, c)
-- inFileWrite fid offset bs fidState me c = do
--   atomically (writeTQueue q bs)
--   return ((Right . fromIntegral . BS.length) bs, c)

data Input = Input KeyCode KeyDirection deriving Show

inputParser :: Parser Input
inputParser = do
  keycode <- decimal
  char ','
  keyDirection <- keyDirectionParser
  return (Input keycode keyDirection)

keyDirectionParser :: Parser KeyDirection
keyDirectionParser =
      (string "Pressed" >> return Pressed)
  <|> (string "Released" >> return Released)
