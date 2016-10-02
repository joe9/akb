#!/bin/sh

input="libxkbcommon/xkbcommon/xkbcommon-names.h"
# output="/tmp/keysymifdefs.txt"

cat <<EOH

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE OverloadedStrings  #-}

module NamesPatterns where

import Foreign

-- https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms

newtype Name = MkName { unName :: String } deriving (Eq, Show)

-- generated using the below commands:
--- sh generate_modifier_names.sh >|NamesPatterns.hsc

EOH

echo ""
echo "#include \"${input}\""
echo ""

grep "^#define XKB_" "${input}" |
    expand --tabs=4 - |
    sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\npattern \2 :: Name\npattern \2 = MkName #{const_str \2} /"
