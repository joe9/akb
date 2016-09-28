
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
--
import Lib

main :: IO ()
main = TIO.putStrLn (showKeySymbol (fromMaybe XK_NoSymbol (fst (getKeySymbol customDvorak 10))))

getKeySymbol :: State -> KeyCode -> (Maybe KeySymbol, State)
getKeySymbol s k = onKeyCode k s
