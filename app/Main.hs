{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Internal         as BSI (c2w)
import           Data.Default
import           Data.Maybe
import           Data.Serialize
import           Data.String.Conversions
import qualified Data.Text.IO                     as TIO
import qualified Data.Vector                      as V
import           Network.Simple.TCP
import           Protolude                        hiding (State,
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
  TIO.putStrLn
    (showKeySymbol ((getKeySymbol (pickInitialState "customDvorak") 10)))
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
      print (keyEvent (pickInitialState "customDvorak") kcode dir)
  case parseOnly inputParser "100,100,1,10,1\r\n" of
    Left e -> print e
    Right (Input _ _ _ kcode dir) ->
      print (keyEvent (pickInitialState "customDvorak") kcode dir)
  case parseOnly inputParser "100,100,1,10,1" of
    Left e -> print e
    Right (Input _ _ _ kcode dir) ->
      print (keyEvent (pickInitialState "customDvorak") kcode dir)
  let context =
        (def :: Context State)
        {cFSItems = fsList, cUserState = pickInitialState "customDvorak"}
  run9PServer context (Host "127.0.0.1") "5960"

getKeySymbol :: State -> KeyCode -> KeySymbol
getKeySymbol s k = fromMaybe XKB_KEY_NoSymbol (lookupKeyCode k s)

-- TODO : add stAtime, stMtime, stUid, stGid and stMuid
-- TODO : use Data.Tree to build this
--   https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#data-tree
fsList :: V.Vector (FSItem (Context State))
fsList =
  V.fromList
    [ directory "/" 0
    , ctlFile "/ctl" 1
    , inFile "/in" 2
    , readOnlyFile "/echo" 3
    , readOnlyFile "/out" 4
    , directory "/modifiers/" 5
    , directory "/modifiers/effective" 6
    , readOnlyFile "/modifiers/effective/out" 7
    , directory "/modifiers/depressed" 8
    , readOnlyFile "/modifiers/depressed/out" 9
    , directory "/modifiers/latched" 10
    , readOnlyFile "/modifiers/latched/out" 11
    , directory "/modifiers/locked" 12
    , readOnlyFile "/modifiers/locked/out" 13
    , directory "/group/" 14
    , directory "/group/effective" 15
    , readOnlyFile "/group/effective/out" 16
    , directory "/group/depressed" 17
    , readOnlyFile "/group/depressed/out" 18
    , directory "/group/latched" 19
    , readOnlyFile "/group/latched/out" 20
    , directory "/group/locked" 21
    , readOnlyFile "/group/locked/out" 22
    ]

inFile :: RawFilePath -> FSItemsIndex -> FSItem (Context State)
inFile name index =
  FSItem Occupied ((writeOnlyFileDetails name index) {dWrite = inFileWrite}) []

inFileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> (Context State)
  -> IO (Either NineError Count, (Context State))
inFileWrite _ _ bs _ _ c = do
  case parseOnly inputParser bs of
    Left e -> return ((Left . OtherError) (BS.append (cs e) bs), c)
    -- write to all /echo read channels
    Right (Input e m t kcode dir) -> do
      writeToOpenChannelsOfFSItemAtIndex 2 bs (cFids c)
      case keyEvent (cUserState c) kcode dir of
        (Nothing, state) -> do
          let fids = cFids c
              kcodebs = (putByteString . show) kcode
              dirvbs = (putByteString . show . fromEnum) dir
              ksbs = (putByteString . show) (0 :: Int)
              modsbs = (putByteString . show . sEffectiveModifiers) state
              utf32bs = (putByteString . show) (0 :: Int)
              utf8bs = (putByteString . show) (0 :: Int)
              ebs = (putByteString . show) e
              mbs = (putByteString . show) m
              tbs = (putByteString . show) t
              rbs = (putByteString . show . fromEnum) False
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
                        , rbs
                        , modsbs
                        , utf32bs
                        , utf8bs
                        ]))
                  (BSI.c2w '\n')
              toModifiersEffectiveOut = BS.snoc (runPut modsbs) (BSI.c2w '\n')
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
          return ((Right . fromIntegral . BS.length) bs, c {cUserState = state})
        (Just (ks, r, mods, utf32, utf8), state) -> do
          let fids = cFids c
              kcodebs = (putByteString . show) kcode
              dirvbs = (putByteString . show . fromEnum) dir -- dirbs = (putByteString . cs . show) dir
              ksbs = (putByteString . show . unKeySymbol) ks
              modsbs = (putByteString . show) mods
              utf32bs = (putByteString . show) utf32
              utf8bs = (putByteString . show) utf8
              ebs = (putByteString . show) e
              mbs = (putByteString . show) m
              tbs = (putByteString . show) t
              rbs = (putByteString . show . fromEnum) r
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
                        , rbs
                        , modsbs
                        , utf32bs
                        , utf8bs
                        ]))
                  (BSI.c2w '\n')
              toModifiersEffectiveOut = BS.snoc (runPut modsbs) (BSI.c2w '\n')
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
          return ((Right . fromIntegral . BS.length) bs, c {cUserState = state})

ctlFile :: RawFilePath -> FSItemsIndex -> FSItem (Context State)
ctlFile name index =
  FSItem Occupied ((writeOnlyFileDetails name index) {dWrite = ctlFileWrite}) []

ctlFileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> (Context State)
  -> IO (Either NineError Count, (Context State))
ctlFileWrite _ _ bs _ _ c = do
  case parseOnly ctlParser bs of
    Left e -> return ((Left . OtherError) (BS.append (cs e) bs), c)
    Right state ->
      return ((Right . fromIntegral . BS.length) bs, c {cUserState = state})

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

ctlParser :: Parser State
ctlParser = do
  initialState <- string "customDvorak" <|> string "customDvorakSticky"
  --   _ <- endOfLine
  _ <- (endOfLine <|> endOfInput)
  return (pickInitialState initialState)
