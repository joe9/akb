#!/bin/sh

input='libxkbcommon/src/keysym-utf.c'
# output="/tmp/keysymifdefs.txt"

cat <<EOH

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE OverloadedStrings  #-}

module KeySymbolToUTF (keySymbolToUTF) where

import Foreign
import KeySymbolDefinitions

-- translated from libxkbcommon/src/keysym-utf.c
keySymbolToUTF :: KeySymbol -> Word32
keySymbolToUTF XKB_KEY_KP_Space = fromIntegral (unKeySymbol XKB_KEY_space) .&. 0x7f
--    /* patch encoding botch */
keySymbolToUTF mks@(MkKeySymbol keysym)
  | (keysym >= 0x0020 && keysym <= 0x007e) ||
        (keysym >= 0x00a0 && keysym <= 0x00ff)
  -- /* first check for Latin-1 characters (1:1 mapping) */
  = fromIntegral keysym

  | (keysym >= unKeySymbol XKB_KEY_BackSpace && keysym <= unKeySymbol XKB_KEY_Clear) ||
        (keysym >= unKeySymbol XKB_KEY_KP_Multiply && keysym <= unKeySymbol XKB_KEY_KP_9) ||
        keysym == unKeySymbol XKB_KEY_Return || keysym == unKeySymbol XKB_KEY_Escape ||
        keysym == unKeySymbol XKB_KEY_Delete || keysym == unKeySymbol XKB_KEY_KP_Tab ||
        keysym == unKeySymbol XKB_KEY_KP_Enter || keysym == unKeySymbol XKB_KEY_KP_Equal
   -- /* special keysyms */
  = fromIntegral keysym .&. 0x7f

  | (keysym .&. 0xff000000) == 0x01000000
  --  /* also check for directly encoded 24-bit UCS characters */
  = fromIntegral keysym .&. 0x00ffffff

  | otherwise = lookupUTF mks

lookupUTF :: KeySymbol -> Word32

-- generated using the below commands:
--- sh generate_key_symbol_definitions.sh >|KeySymbolToUTF.hs

EOH

sed --expression='1,/struct codepair keysymtab/d' "${input}" |
    sed --expression='/};/,$d' |
    sed --expression='s,^\([[:space:]]*/\),-- \1,g' |
    sed --expression='s/\([^{]*{ \([^,]*\), \(\S*\).*\)/lookupUTF (MkKeySymbol \2) =  \3 -- \1/g'

echo "lookupUTF _ = 0"
