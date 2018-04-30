Ada Bitcoin Script Interpreter
==============================

This is the start of a bitcoin script interpreter written in the highly reliable language Ada.

Planned Features:

- Evaluate all Bitcoin Script opcodes
- Base58 and Base64 Encoding
- Focused OpenSSL binding for secp256k1 and ripemd160

Differences to the standard:

- Nested If/Else blocks are supported
- OP_VERIF and OP_VERNOTIF will only invalidate if they are evaluated
- Segwit is not, and never will be, supported.

## Compiling

There are 4 build options available:

- Word_Size : 32 or 64
- System : windows or unix
- Debug : Yes or No
- Test : any file from the tests folder

To compile from command line with all the defaults (32, windows, No, script_test.adb):

`gprbuild -P bitcoin_script.gpr`

To compile from command line with specific options

`gprbuild -P bitcoin_script.gpr -XWord_Size=32 -XSystem=windows -XTest=script_test.adb -XDebug=Yes`

Or the free IDE [Gnat Programming Studio](http://libre.adacore.com/download/) can open and compile gpr files 

## Dependencies

- OpenSSL

Windows: I've included the latest 1.1.0 openSSL binaries since they are a little difficult to find. The gpr file assumes the dlls will be in the binaries/system/word_size folder.

Linux: Use your package manager to install the openssl dev libraries. For ubuntu based systems: `sudo apt-get install libssl-dev`

## Contributing

This project will use gitfow.

To contribute, create a feature branch off develop. Example feature/ripemd160-support. 
When you're ready, open a pull request to merge back into develop, I'll review the changes.

Things to work on:

- Opcodes
  - Stack
  - Splice
  - Bitwise Logic
  - Arithmetic
  - Crypto
  - Locktime
  - Pseudo-words
