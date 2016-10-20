{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM.TQueue
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import qualified Data.HashMap.Strict              as HashMap
import           Data.Maybe
import           Data.Serialize
import           Data.Serialize.Put
import           Data.String.Conversions
import qualified Data.Text.IO                     as TIO
import qualified Data.Vector                      as V
import           Data.Word
import           GHC.Show
import           Protolude                        hiding (State, show)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Text.Groom

import           BitMask
import           Data.NineP            hiding (Directory)
import           Network.NineP.Context hiding (File)
import qualified Network.NineP.Context as Context
import           Network.NineP.Error
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
    [ dir "/" -- 0
    , file "/in" -- 1
    , file "/out" -- 2
    , dir "/modifiers/" -- 3
    , dir "/modifiers/effective" -- 4
    , file "/modifiers/effective/out" -- 5
    , dir "/modifiers/depressed" -- 6
    , file "/modifiers/depressed/out" -- 7
    , dir "/modifiers/latched" -- 8
    , file "/modifiers/latched/out" -- 9
    , dir "/modifiers/locked" -- 10
    , file "/modifiers/locked/out" -- 11
    , dir "/group/" -- 12
    , dir "/group/effective" -- 13
    , file "/group/effective/out" -- 14
    , dir "/group/depressed" -- 15
    , file "/group/depressed/out" -- 16
    , dir "/group/latched" -- 17
    , file "/group/latched/out" -- 18
    , dir "/group/locked" -- 19
    , dir "/group/locked/out" -- 20
    ]

--     , FSItem Context.File
--        ((fileDetails  "/group/locked/out") { dRead = }) []
dir, file :: RawFilePath -> FSItem Context
dir name = FSItem Directory (dirDetails name) []

file name = FSItem Context.File (fileDetails name) []

inFileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> Context
  -> IO (Either NineError Count, Context)
inFileWrite fid offset bs fidState me c = do
  case parseOnly inputParser bs of
    Left e -> return ((Left . OtherError) (BS.append (cs e) bs), c)
    Right (Input kcode dir) -> do
      case keyEvent (pickInitialState 1) kcode dir of
        (Nothing, state) -> return ((Right . fromIntegral . BS.length) bs, c)
        (Just (ks, mods), state) -> do
          let fids = cFids c
              kcodebs = (putWord32le kcode)
              dirbs = (putByteString . cs . show) dir
              ksbs = (putWord32le . unKeySymbol) ks
              modsbs = (putWord32le mods)
              toOut0 =
                BS.intercalate "," (fmap runPut [kcodebs, dirbs, ksbs, modsbs])
              toOut5 = runPut modsbs
          -- write to all /out read channels
          writeToOpenChannelsOfFSItemAtIndex 0 toOut0 fids
          -- write to all /modifiers/effective/out read channels
          writeToOpenChannelsOfFSItemAtIndex 5 toOut5 fids
          -- write to all /modifiers/depressed/out read channels
          let toOut7 = (runPut . putWord32le . sDepressedModifiers) state
          writeToOpenChannelsOfFSItemAtIndex 7 toOut7 fids
          -- write to all /modifiers/latched/out read channels
          let toOut9 = (runPut . putWord32le . sLatchedModifiers) state
          writeToOpenChannelsOfFSItemAtIndex 9 toOut9 fids
          -- write to all /modifiers/locked/out read channels
          let toOut11 = (runPut . putWord32le . sLockedModifiers) state
          writeToOpenChannelsOfFSItemAtIndex 11 toOut11 fids
          -- write to all /group/effective/out read channels
          let toOut14 = (runPut . putWord32le . sEffectiveGroup) state
          writeToOpenChannelsOfFSItemAtIndex 14 toOut14 fids
          -- write to all /group/depressed/out read channels
          let toOut16 = (runPut . putWord32le . sDepressedGroup) state
          writeToOpenChannelsOfFSItemAtIndex 16 toOut16 fids
          -- write to all /group/latched/out read channels
          let toOut18 = (runPut . putWord32le . sLatchedGroup) state
          writeToOpenChannelsOfFSItemAtIndex 18 toOut18 fids
          -- write to all /group/locked/out read channels
          let toOut20 = (runPut . putWord32le . sLockedGroup) state
          writeToOpenChannelsOfFSItemAtIndex 20 toOut20 fids
          return ((Right . fromIntegral . BS.length) bs, c)

data Input =
  Input KeyCode
        KeyDirection
  deriving (Show)

inputParser :: Parser Input
inputParser = do
  keycode <- decimal
  char ','
  keyDirection <- keyDirectionParser
  return (Input keycode keyDirection)

keyDirectionParser :: Parser KeyDirection
keyDirectionParser =
  (string "Pressed" >> return Pressed) <|>
  (string "Released" >> return Released)

writeToOpenChannelsOfFSItemAtIndex :: FSItemsIndex
                                   -> ByteString
                                   -> HashMap.HashMap Fid FidState
                                   -> IO ()
writeToOpenChannelsOfFSItemAtIndex i bs =
  mapM_ (\f -> writeToMaybeQueue (fidQueue f) bs) .
  HashMap.filter (\f -> i == fidFSItemsIndex f && isJust (fidQueue f))

writeToMaybeQueue :: Maybe (TQueue ByteString) -> ByteString -> IO ()
writeToMaybeQueue (Nothing) _ = return ()
writeToMaybeQueue (Just q) bs = atomically (writeTQueue q bs)
