# akb

Understandable non-backward-compatible alternative to libxkbcommon with working Sticky keys.

Use the src/Bindings/Xkbcomon.hsc to guide the Haskell FFI development

* cd src/Bindings && c2hsc --prefix=Bindings ../libxkbcommon/xkbcommon/xkbcommon.h

## Installation

Build Instructions:

1.   Use make instead of "stack build" as make generates the custom libskb.so

Install instructions:

1. Install libxkbcommon
2. Install skb using make && make install. Below is what install does.
    * cp src/libskb.so $(WLD)/lib/
    * cp src/libskb-xkbcommon.so $(WLD)/lib/
    * cp src/Skb_stub.h $(WLD)/include/
    * cp src/skb.h $(WLD)/include/
3. cd (lib directory)/
    * mv libxkbcommon.so.0.0.0 libxkbcommon.so.0.0.0.original
    * ln -sf libskb-xkbcommon.so libxkbcommon.so.0.0.0

## Usage

TODO: Write usage instructions

## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D

## History

Felt that it would be easier to roll this out than trying to add the sticky functionality to libxkbcommon. Also, it was frustrating working with xkb sometime ago, as the xkb files are quasi-programs. xkb is a parser, interpreter and a lookup function. It is easier to encode the keycode->keysym into the program as a lookup table and use functions to configure how you want xkb to use that lookup table (which is what skb does).

## Credits

Special thanks to Cloudef of #orbment, daniels of #wayland

## License

TODO: Write license
