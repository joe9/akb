
module Main where

import Lib

main :: IO ()
main = print (fst (getKeySymbol customDvorak 10))

getKeySymbol :: State -> KeyCode -> (Maybe KeySymbol, State)
getKeySymbol s k = onKeyCode k s
