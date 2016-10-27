# akb

Alternative Keyboard to use with arcan. Designed to work with sticky keys.

## Installation

Build Instructions:

1.  build akb using stack or cabal

Install instructions:

1. State the Akb process (opens a tcp port 5960)
2. Mount the above process as filesystem tree
    * mkdir /home/j/dev/apps/durden-arcan/kbdfs
    * sudo 9mount 'tcp!localhost!5960' /home/j/dev/apps/durden-arcan/kbdfs
    * Or, for debugging, sudo 9mount -d err,devel,9p,vfs,conv,fcall 'tcp!localhost!5960' /home/j/dev/apps/durden-arcan/kbdfs
3. patch arcan to use the above functionality
    * TODO
    * TODO

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

Special thanks to the God of Arcan/Durden

## License

TODO: Write license
