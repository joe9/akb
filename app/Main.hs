{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import qualified Data.ByteString.Internal         as BSI (c2w)
import           Data.Default
import           Data.Maybe
import           Data.Serialize
import           Data.String.Conversions
import qualified Data.Text.IO                     as TIO
import qualified Data.Vector                      as V
import           GHC.Show
import           Network.Simple.TCP
import           Protolude                        hiding (State, show,
                                                   state)
import           System.Posix.ByteString.FilePath

-- import           Text.Groom
import BitMask

import Data.NineP
import Network.NineP.Context
import Network.NineP.Directory
import Network.NineP.Error
import Network.NineP.ReadOnlyFile
import Network.NineP.Server
import Network.NineP.WriteOnlyFile

-- import           Network.NineP.Functions
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
  case parseOnly inputParser "100,100,1,10,1\n" of
    Left e -> print e
    Right (Input _ _ _ kcode dir) ->
      print (keyEvent (pickInitialState 1) kcode dir)
  case parseOnly inputParser "100,100,1,10,1\r\n" of
    Left e -> print e
    Right (Input _ _ _ kcode dir) ->
      print (keyEvent (pickInitialState 1) kcode dir)
  case parseOnly inputParser "100,100,1,10,1" of
    Left e -> print e
    Right (Input _ _ _ kcode dir) ->
      print (keyEvent (pickInitialState 1) kcode dir)
  let context = def {cFSItems = fsList}
  run9PServer context (Host "127.0.0.1") "5960"

getKeySymbol :: State -> KeyCode -> KeySymbol
getKeySymbol s k = lookupKeyCode k s

-- TODO : add stAtime, stMtime, stUid, stGid and stMuid
-- TODO : use Data.Tree to build this
--   https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#data-tree
fsList :: V.Vector (FSItem Context)
fsList =
  V.fromList
    [ directory "/" 0
    , inFile "/in" 1
    , readOnlyFile "/echo" 2
    , readOnlyFile "/out" 3
    , directory "/modifiers/" 4
    , directory "/modifiers/effective" 5
    , readOnlyFile "/modifiers/effective/out" 6
    , directory "/modifiers/depressed" 7
    , readOnlyFile "/modifiers/depressed/out" 8
    , directory "/modifiers/latched" 9
    , readOnlyFile "/modifiers/latched/out" 10
    , directory "/modifiers/locked" 11
    , readOnlyFile "/modifiers/locked/out" 12
    , directory "/group/" 13
    , directory "/group/effective" 14
    , readOnlyFile "/group/effective/out" 15
    , directory "/group/depressed" 16
    , readOnlyFile "/group/depressed/out" 17
    , directory "/group/latched" 18
    , readOnlyFile "/group/latched/out" 19
    , directory "/group/locked" 20
    , readOnlyFile "/group/locked/out" 21
    ]

inFile :: RawFilePath -> FSItemsIndex -> FSItem Context
inFile name index =
  FSItem Occupied ((writeOnlyFileDetails name index) {dWrite = inFileWrite}) []

inFileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> Context
  -> IO (Either NineError Count, Context)
inFileWrite _ _ bs _ _ c = do
  case parseOnly inputParser bs of
    Left e -> return ((Left . OtherError) (BS.append (cs e) bs), c)
    -- write to all /echo read channels
    Right (Input e m t kcode dir) -> do
      writeToOpenChannelsOfFSItemAtIndex 2 bs (cFids c)
      case keyEvent (pickInitialState 1) kcode dir of
        (Nothing, state) ->
          BSC.putStrLn "inFileWrite: lookup returned nothing" >>
          return ((Right . fromIntegral . BS.length) bs, c)
        (Just (ks, mods, utf32, utf8), state) -> do
          let fids = cFids c
              kcodebs = (putByteString . cs . show) kcode
              dirvbs = (putByteString . cs . show . fromEnum) dir -- dirbs = (putByteString . cs . show) dir
              ksbs = (putByteString . cs . show . unKeySymbol) ks
              modsbs = (putByteString . cs . show) mods
              utf32bs = (putByteString . cs . show) utf32
              utf8bs = (putByteString . cs . show) utf8
              ebs = (putByteString . cs . show) e
              mbs = (putByteString . cs . show) m
              tbs = (putByteString . cs . show) t
              toOut =
                BS.snoc
                  (BS.intercalate
                     ","
                     (fmap
                        runPut
                        [ ebs
                        , mbs
                        , tbs
                        , kcodebs
                        , dirvbs
                        , ksbs
                        , modsbs
                        , utf32bs
                        , utf8bs
                        ]))
                  (BSI.c2w '\n')
              toModifiersEffectiveOut = runPut modsbs
          -- write to all /out read channels
          writeToOpenChannelsOfFSItemAtIndex 3 toOut fids
          -- write to all /modifiers/effective/out read channels
          writeToOpenChannelsOfFSItemAtIndex 6 toModifiersEffectiveOut fids
          -- write to all /modifiers/depressed/out read channels
          let toModifiersDepressedOut =
                (runPut . putWord32le . sDepressedModifiers) state
          writeToOpenChannelsOfFSItemAtIndex 8 toModifiersDepressedOut fids
          -- write to all /modifiers/latched/out read channels
          let toModifiersLatchedOut =
                (runPut . putWord32le . sLatchedModifiers) state
          writeToOpenChannelsOfFSItemAtIndex 10 toModifiersLatchedOut fids
          -- write to all /modifiers/locked/out read channels
          let toModifiersLockedOut =
                (runPut . putWord32le . sLockedModifiers) state
          writeToOpenChannelsOfFSItemAtIndex 12 toModifiersLockedOut fids
          -- write to all /group/effective/out read channels
          let toGroupEffectiveOut =
                (runPut . putWord32le . sEffectiveGroup) state
          writeToOpenChannelsOfFSItemAtIndex 15 toGroupEffectiveOut fids
          -- write to all /group/depressed/out read channels
          let toGroupDepressedOut =
                (runPut . putWord32le . sDepressedGroup) state
          writeToOpenChannelsOfFSItemAtIndex 17 toGroupDepressedOut fids
          -- write to all /group/latched/out read channels
          let toGroupLatchedOut = (runPut . putWord32le . sLatchedGroup) state
          writeToOpenChannelsOfFSItemAtIndex 19 toGroupLatchedOut fids
          -- write to all /group/locked/out read channels
          let toGroupLockedOut = (runPut . putWord32le . sLockedGroup) state
          writeToOpenChannelsOfFSItemAtIndex 21 toGroupLockedOut fids
          return ((Right . fromIntegral . BS.length) bs, c)

-- data Input =
--   Input KeyCode
--         KeyDirection
--   deriving (Show)
-- inputParser :: Parser Input
-- inputParser = do
--   keycode <- decimal
--   _ <- char ','
--   keyDirection <- keyDirectionParserS
--   return (Input keycode keyDirection)
-- keyDirectionParserS :: Parser KeyDirection
-- keyDirectionParserS =
--   (string "Pressed" >> return Pressed) <|>
--   (string "Released" >> return Released)
-- from /usr/include/linux/input.h : input_event
-- input_event
--       = long int time in seconds 64 bits,
--         long int time in microseconds 64 bits,
--         unsigned 16 bits type,
--         unsigned 16 bits code,
--         signed 32 bits value,
-- read these second values in. it will be easy to check if the input
-- matches with the output by the calling application
data Input = Input
  { iEpochSeconds :: Integer
  , iMicroseconds :: Integer
  , iType         :: Word16
  , iKeyCode      :: KeyCode
  , iKeyDirection :: KeyDirection
  } deriving (Eq, Show)

-- Do I need the endOfInput matching below?
inputParser :: Parser Input
inputParser = do
  epoch <- decimal
  _ <- char ','
  microseconds <- decimal
  _ <- char ','
  itype <- decimal
  _ <- char ','
  keyCode <- decimal
  _ <- char ','
  value <- decimal
  --   _ <- endOfLine
  _ <- (endOfLine <|> endOfInput)
  return (Input epoch microseconds itype keyCode (toEnum value))
