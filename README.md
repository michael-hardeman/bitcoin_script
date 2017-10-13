Ada Bitcoin Script Interpreter
==============================

This is the start of a bitcoin scirpt interpreter written in the highly reliable language Ada.

Planned Features:

- Evaluate all Bitcoin Script opcodes
- Base58 and Base64 Encoding
- Focused OpenSSL binding for secp256k1 and ripemd160

Differences to the standard:

- Nested If/Else blocks are supported
- OP_VERIF and OP_VERNOTIF will only invalidate if they are evaluated

## Compiling

There are 4 building Options available:
  Word_Size : 32 or 64
  System    : windows or unix
  Debug     : Yes or No
  Test      : any file from the tests folder

To compile from command line with all the defaults (32, windows, No, bitcoin_script.adb):

`gprbuild -P bitcoin_script.gpr`

To compile from command line with specific options

`gprbuild -P bitcoin_script.gpr -XWord_Size=32 -XSystem=windows -XTest=script_test.adb -XDebug=Yes`

Or the free IDE [Gnat Programming Studio](http://libre.adacore.com/download/) can open and compile gpr files 

## Dependencies

- OpenSSL

Windows: I've included the latest 1.1.0 openSSL binaries. The gpr file assumes the dlls will be in the binaries\\system\\word_size folder.

Linux: You'll have to find your own. You can usually install libssl with your package manager. The gpr file assumes the binaries will be on the path.

## Contributing

This project will use gitfow.

To contribute, create a feature branch off develop. Example feature/ripemd160-support. 
When you're ready, open a pull request to merge back into develop, I'll review the changes.

Things to work on:

- OpenSSL binding
  - Proper initialization
  - Proper finalization
  - RIPEMD160 binding
- Opcodes
  - Stack
  - Splice
  - Bitwise Logic
  - Arithmetic
  - Crypto
  - Locktime
  - Pseudo-words
