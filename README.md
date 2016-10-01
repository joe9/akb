# skb

Understandable non-backward-compatible alternative to libxkbcommon

src/Bindings/Xkbcomon.hsc files is generated using:

1. cd src/Bindings && c2hsc --prefix=Bindings ../libxkbcommon/xkbcommon/xkbcommon.h
2. comment out code causing build errors
3. implement all #ccall's using ffi "export" functions in haskell and comment out all the #ccal's as the generated .h file would have all the export functions.

## Installation

Build Instructions:
1.   Use make instead of "stack build" as make generates the custom libskb.so

Install instructions:
1. Install libxkbcommon
2. Install skb
3. cd <include directory>/libxkbcommon, ln -sf skb.h libxkbcommon.h
4. It is probably not a good idea to do the above, instead, leave skb and libxkbcommon as 2 different entities with some common structures.

## Usage

TODO: Write usage instructions

## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D

## History

TODO: Write history

## Credits

Special thanks to Cloudef of #orbment, daniels of #wayland

## License

TODO: Write license
