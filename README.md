# skb
Understandable non-backward-compatible alternative to libxkbcommon

src/Bindings/Xkbcomon.hsc files is generated using:
  cd src/Bindings && c2hsc --prefix=Bindings ../libxkbcommon/xkbcommon/xkbcommon.h
  comment out code causing build errors
  implement all #ccall's using ffi "export" functions in haskell and comment out all the #ccal's as the generated .h file would have all the export functions.

Build Instructions:
   Use make instead of "stack build" as make generates the custom libskb.so

Install instructions:
  Install libxkbcommon
  Install skb
  cd <include directory>/libxkbcommon, ln -sf skb.h libxkbcommon.h
