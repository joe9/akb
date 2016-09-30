#!/bin/sh

input="libxkbcommon/xkbcommon/xkbcommon-keysyms.h"
# output="/tmp/keysymifdefs.txt"

cat <<EOH

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE OverloadedStrings  #-}

module KeySymbolDefinitions where

import Foreign
import qualified Data.Text as T

newtype KeySymbol = MkKeySymbol { unKeySymbol :: Word32 } deriving (Eq, Show)

-- need to convert the Text to String to use the below
-- instance show KeySymbol where show = showKeySymbol

-- https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms

-- generated using the below commands:
--- sh generate_key_symbol_definitions.sh >|/tmp/keysymsout.hsc


EOH

echo ""
echo "#include \"${input}\""
echo ""

# echo "data KeySymbol = XK_NoSymbol "
# grep "^#define" "${input}" |
#     sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\n   | \2/"
# echo "  deriving Eq"
# echo ""
# echo "-- instance Show KeySymbol where show = showKeySymbol"
# echo ""
# echo "toKeySymbol :: KeySymbol -> CInt"
# echo "toKeySymbol XK_NoSymbol = 0"

# grep "^#define" "${input}" |
#     sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\ntoKeySymbol \2 = #{const \2}/"
# echo "toKeySymbol _ = toKeySymbol XK_VoidSymbol"

# echo ""
# echo "fromKeySymbol :: CInt -> KeySymbol"
# echo "fromKeySymbol 0 = XK_NoSymbol"

# grep "^#define" "${input}" |
#     sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\nfromKeySymbol #{const \2} = \2/"
# echo "fromKeySymbol _ = XK_VoidSymbol"

grep "^#define XKB_KEY_" "${input}" |
    expand --tabs=4 - |
    sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\npattern \2 :: KeySymbol\npattern \2 = MkKeySymbol #{const \2} /"

echo ""
echo "showKeySymbol :: KeySymbol -> T.Text"
grep "^#define XKB_KEY_" "${input}" |
    expand --tabs=4 - |
    sed --expression="s/\(^#define \(XKB_KEY_\(\S*\)\) .*\)/-- \1\nshowKeySymbol \2 = \"\3\"/"
echo "showKeySymbol _ = \"Unknown symbol\""
