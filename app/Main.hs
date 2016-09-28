
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified Data.Text.IO as TIO
import Data.Maybe
--
import Lib

main :: IO ()
main = TIO.putStrLn (showKeySymbol (fromMaybe noKeySymbol (fst (getKeySymbol customDvorak 11))))

getKeySymbol :: State -> KeyCode -> (Maybe KeySymbol, State)
getKeySymbol s k = onKeyCode k s
