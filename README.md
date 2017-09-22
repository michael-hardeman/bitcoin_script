Ada Bitcoin Script Interpreter
==============================

NOTE: THIS PROJECT IS MOSTLY INCOMPLETE

This is the start of a bitcoin scirpt interpreter written in the highly reliable language Ada.

Planned Features:

- Evaluate all Bitcoin Script opcodes
- Focused OpenSSL binding for cryptography

## Compiling

There are 4 building Options available:
  Word_Size : 32 or 64
  System    : windows or unix
  Debug     : Yes or No
  Test      : any file from the tests folder

To compile from command line with all the defaults (32, windows, No, bitcoin_script.adb):

`gprbuild -P bitcoin_script.gpr`

To compile from command line with specific options

`gprbuild -P bitcoin_script.gpr -XWord_Size=32 -XSystem=windows -XTest=bitcoin_script_test.adb -XDebug=No`

Or the free IDE [Gnat Programming Studio](http://libre.adacore.com/download/) can open and compile gpr files.

## Dependencies

- OpenSSL

Windows: I've included the latest 1.1.0 openSSL binaries. The gpr file assumes the dlls will be in the binaries\\system\\word_size folder.

Linux: You'll have to find your own. You can usually install libssl with your package manager. The gpr file assumes the binaries will be on the path.

## Contributing

This project will use gitfow.

Things to work on:

- OpenSSL binding
  - Base64 Encoding/Decoding
  - Base58 Encoding/Decoding
  - RIPEMD160 binding
- Opcodes
  - All Constant and Flow Control Opcodes have been implemented.
  - All the rest need implementation.
