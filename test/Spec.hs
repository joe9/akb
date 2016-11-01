{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Safe
import           Data.String.Conversions
import           Protolude                        hiding (bracket)
import           System.IO                        hiding
                                                   (withBinaryFile)
import qualified System.IO                        as IO
import           System.Posix.ByteString.FilePath
import           Test.Tasty                       (defaultMain,
                                                   testGroup)

import qualified State.Tests

-- import           FileSystem.Tests
-- import qualified Keymap.CustomDvorak.Tests
main :: IO ()
main = do
  withBinaryFile
    "/home/j/dev/apps/durden-arcan/kbdfs/out"
    ReadMode
    (\outHandle -> do
       withBinaryFile
         "/home/j/dev/apps/durden-arcan/kbdfs/modifiers/effective/out"
         ReadMode
         (\effectiveModifiersOutHandle -> do
            withBinaryFile
              "/home/j/dev/apps/durden-arcan/kbdfs/in"
              WriteMode
              (\inHandle -> do
                 defaultMain
                   (testGroup
                      "Tests"
                      [ State.Tests.tests
                          outHandle
                          effectiveModifiersOutHandle
                          inHandle
                      ]))))

-- below from System.IO
-- | @'withBinaryFile' name mode act@ opens a file using 'openBinaryFile'
-- and passes the resulting handle to the computation @act@.  The handle
-- will be closed on exit from 'withBinaryFile', whether by normal
-- termination or by raising an exception.
withBinaryFile :: RawFilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile name mode = bracket (IO.openBinaryFile (cs name) mode) hClose
