#!/bin/sh

input="xproto/keysymdef.h"
# output="/tmp/keysymifdefs.txt"

grep "^#ifdef" "${input}" |
    sed --expression="s/\(^#ifdef \(\S*\).*\)/-- \1\n#define \2 1/"

echo ""
echo "#include \"${input}\""
echo ""
echo "data KeySymbol = XK_NoSymbol "

grep "^#define" "${input}" |
    sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\n   | \2/"
echo "  deriving Eq"
echo ""
echo "-- instance Show KeySymbol where show = showKeySymbol"
echo ""
echo "toKeySymbol :: KeySymbol -> CInt"
echo "toKeySymbol XK_NoSymbol = 0"

grep "^#define" "${input}" |
    sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\ntoKeySymbol \2 = #{const \2}/"
echo "toKeySymbol _ = toKeySymbol XK_VoidSymbol"

echo ""
echo "fromKeySymbol :: CInt -> KeySymbol"
echo "fromKeySymbol 0 = XK_NoSymbol"

grep "^#define" "${input}" |
    sed --expression="s/\(^#define \(\S*\) .*\)/-- \1\nfromKeySymbol #{const \2} = \2/"
echo "fromKeySymbol _ = XK_VoidSymbol"

echo ""
echo "showKeySymbol :: KeySymbol -> Text"
echo "showKeySymbol XK_NoSymbol = \"XK_NoSymbol\""
grep "^#define" "${input}" |
    sed --expression="s/\(^#define \(XK_\(\S*\)\) .*\)/-- \1\nshowKeySymbol \2 = \"\3\"/"
echo "showKeySymbol _ = \"Unknown symbol\""
