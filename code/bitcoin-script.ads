with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Finalization;
with Ada.Containers;
with Bitcoin.Data.Stacks;

package Bitcoin.Script is

  --------------
  -- Packages --
  --------------
  package Byte_Array_Stacks is new Bitcoin.Data.Stacks (Positive, Byte_Array, "=");

  --------------
  -- Op Codes --
  --------------
  type Opcode_Kind is (

    ---------------
    -- Constants --
    ---------------
    OP_0,         OP_PUSHDATA1, OP_PUSHDATA2,
    OP_PUSHDATA4, OP_1NEGATE,   OP_RESERVED,
    OP_1,         OP_2,         OP_3,
    OP_4,         OP_5,         OP_6,
    OP_7,         OP_8,         OP_9,
    OP_10,        OP_11,        OP_12,
    OP_13,        OP_14,        OP_15,
    OP_16,

    ------------------
    -- Flow Control --
    ------------------
    OP_NOP,    OP_VER,
    OP_IF,     OP_NOTIF,
    OP_VERIF,  OP_VERNOTIF,
    OP_ELSE,   OP_ENDIF,
    OP_VERIFY, OP_RETURN,

    -----------
    -- Stack --
    -----------
    OP_TOALTSTACK, OP_FROMALTSTACK, OP_2DROP, OP_2DUP,  OP_3DUP,
    OP_2OVER,      OP_2ROT,         OP_2SWAP, OP_IFDUP, OP_DEPTH,
    OP_DROP,       OP_DUP,          OP_NIP,   OP_OVER,  OP_PICK,
    OP_ROLL,       OP_ROT,          OP_SWAP,  OP_TUCK,

    ------------
    -- Splice --
    ------------
    OP_CAT,   OP_SUBSTR, OP_LEFT,
    OP_RIGHT, OP_SIZE,

    -------------------
    -- Bitwise Logic --
    -------------------
    OP_INVERT, OP_AND,         OP_OR,        OP_XOR,
    OP_EQUAL,  OP_EQUALVERIFY, OP_RESERVED1, OP_RESERVED2,

    ----------------
    -- Arithmetic --
    ----------------
    OP_1ADD,           OP_1SUB,            OP_2MUL,
    OP_2DIV,           OP_NEGATE,          OP_ABS,
    OP_NOT,            OP_0NOTEQUAL,       OP_ADD,
    OP_SUB,            OP_MUL,             OP_DIV,
    OP_MOD,            OP_LSHIFT,          OP_RSHIFT,
    OP_BOOLAND,        OP_BOOLOR,          OP_NUMEQUAL,
    OP_NUMEQUALVERIFY, OP_NUMNOTEQUAL,     OP_LESSTHAN,
    OP_GREATERTHAN,    OP_LESSTHANOREQUAL, OP_GREATERTHANOREQUAL,
    OP_MIN,            OP_MAX,             OP_WITHIN,

    ------------
    -- Crypto --
    ------------
    OP_RIPEMD160,     OP_SHA1,     OP_SHA256,         OP_HASH160,       OP_HASH256,
    OP_CODESEPARATOR, OP_CHECKSIG, OP_CHECKSIGVERIFY, OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY,

    --------------
    -- Locktime --
    --------------
    OP_NOP1, OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY, OP_NOP4, OP_NOP5,
    OP_NOP6, OP_NOP7,                OP_NOP8,                OP_NOP9, OP_NOP10,

    ------------------
    -- Pseudo-words --
    ------------------
    OP_PUBKEYHASH, OP_PUBKEY, OP_INVALIDOPCODE);

  for Opcode_Kind use (

    ---------------
    -- Constants --
    ---------------
    OP_0         => 16#00#, OP_PUSHDATA1 => 16#4C#, OP_PUSHDATA2 => 16#4D#,
    OP_PUSHDATA4 => 16#4E#, OP_1NEGATE   => 16#4F#, OP_RESERVED  => 16#50#,
    OP_1         => 16#51#, OP_2         => 16#52#, OP_3         => 16#53#,
    OP_4         => 16#54#, OP_5         => 16#55#, OP_6         => 16#56#,
    OP_7         => 16#57#, OP_8         => 16#58#, OP_9         => 16#59#,
    OP_10        => 16#5A#, OP_11        => 16#5B#, OP_12        => 16#5C#,
    OP_13        => 16#5D#, OP_14        => 16#5E#, OP_15        => 16#5F#,
    OP_16        => 16#60#,

    ------------------
    -- Flow control --
    ------------------
    OP_NOP    => 16#61#, OP_VER      => 16#62#,
    OP_IF     => 16#63#, OP_NOTIF    => 16#64#,
    OP_VERIF  => 16#65#, OP_VERNOTIF => 16#66#,
    OP_ELSE   => 16#67#, OP_ENDIF    => 16#68#,
    OP_VERIFY => 16#69#, OP_RETURN   => 16#6A#,

    -----------
    -- Stack --
    -----------
    OP_TOALTSTACK => 16#6B#, OP_FROMALTSTACK => 16#6C#, OP_2DROP => 16#6D#, OP_2DUP  => 16#6E#,
    OP_3DUP       => 16#6F#, OP_2OVER        => 16#70#, OP_2ROT  => 16#71#, OP_2SWAP => 16#72#,
    OP_IFDUP      => 16#73#, OP_DEPTH        => 16#74#, OP_DROP  => 16#75#, OP_DUP   => 16#76#,
    OP_NIP        => 16#77#, OP_OVER         => 16#78#, OP_PICK  => 16#79#, OP_ROLL  => 16#7A#,
    OP_ROT        => 16#7B#, OP_SWAP         => 16#7C#, OP_TUCK  => 16#7D#,

    ------------
    -- Splice --
    ------------
    OP_CAT   => 16#7E#, OP_SUBSTR => 16#7F#, OP_LEFT => 16#80#,
    OP_RIGHT => 16#81#, OP_SIZE   => 16#82#,

    -------------------
    -- Bitwise logic --
    -------------------
    OP_INVERT    => 16#83#, OP_AND         => 16#84#,
    OP_OR        => 16#85#, OP_XOR         => 16#86#,
    OP_EQUAL     => 16#87#, OP_EQUALVERIFY => 16#88#,
    OP_RESERVED1 => 16#89#, OP_RESERVED2   => 16#8A#,

    ----------------
    -- Arithmetic --
    ----------------
    OP_1ADD           => 16#8B#, OP_1SUB            => 16#8C#, OP_2MUL               => 16#8D#,
    OP_2DIV           => 16#8E#, OP_NEGATE          => 16#8F#, OP_ABS                => 16#90#,
    OP_NOT            => 16#91#, OP_0NOTEQUAL       => 16#92#, OP_ADD                => 16#93#,
    OP_SUB            => 16#94#, OP_MUL             => 16#95#, OP_DIV                => 16#96#,
    OP_MOD            => 16#97#, OP_LSHIFT          => 16#98#, OP_RSHIFT             => 16#99#,
    OP_BOOLAND        => 16#9A#, OP_BOOLOR          => 16#9B#, OP_NUMEQUAL           => 16#9C#,
    OP_NUMEQUALVERIFY => 16#9D#, OP_NUMNOTEQUAL     => 16#9E#, OP_LESSTHAN           => 16#9F#,
    OP_GREATERTHAN    => 16#A0#, OP_LESSTHANOREQUAL => 16#A1#, OP_GREATERTHANOREQUAL => 16#A2#,
    OP_MIN            => 16#A3#, OP_MAX             => 16#A4#, OP_WITHIN             => 16#A5#,

    ------------
    -- Crypto --
    ------------
    OP_RIPEMD160     => 16#A6#, OP_SHA1                => 16#A7#,
    OP_SHA256        => 16#A8#, OP_HASH160             => 16#A9#,
    OP_HASH256       => 16#AA#, OP_CODESEPARATOR       => 16#AB#,
    OP_CHECKSIG      => 16#AC#, OP_CHECKSIGVERIFY      => 16#AD#,
    OP_CHECKMULTISIG => 16#AE#, OP_CHECKMULTISIGVERIFY => 16#AF#,

    --------------
    -- Locktime --
    --------------
    OP_NOP1                => 16#B0#, OP_CHECKLOCKTIMEVERIFY => 16#B1#,
    OP_CHECKSEQUENCEVERIFY => 16#B2#, OP_NOP4                => 16#B3#,
    OP_NOP5                => 16#B4#, OP_NOP6                => 16#B5#,
    OP_NOP7                => 16#B6#, OP_NOP8                => 16#B7#,
    OP_NOP9                => 16#B8#, OP_NOP10               => 16#B9#,

    ------------------
    -- Pseudo-words --
    ------------------
    OP_PUBKEYHASH    => 16#FD#,
    OP_PUBKEY        => 16#FE#,
    OP_INVALIDOPCODE => 16#FF#);

  for Opcode_Kind'Size use Byte'Size;

  -------------
  -- Aliases --
  -------------
  OP_FALSE : constant Opcode_Kind := OP_0;
  OP_TRUE  : constant Opcode_Kind := OP_1;
  function OP_PUSHDATA (Quantity : in Byte) return Byte is (Quantity);

  -----------------------
  -- Contiguous Groups --
  -----------------------
  subtype Constants_Opcode_Kind    is Opcode_Kind range OP_0          .. OP_16;
  subtype Flow_Control_Opcode_Kind is Opcode_Kind range OP_NOP        .. OP_RETURN;
  subtype Stack_Opcode_Kind        is Opcode_Kind range OP_TOALTSTACK .. OP_TUCK;
  subtype Bitwise_Opcode_Kind      is Opcode_Kind range OP_INVERT     .. OP_RESERVED2;
  subtype Arithmetic_Opcode_Kind   is Opcode_Kind range OP_1ADD       .. OP_WITHIN;
  subtype Crypto_Opcode_Kind       is Opcode_Kind range OP_RIPEMD160  .. OP_CHECKMULTISIGVERIFY;
  subtype Locktime_Opcode_Kind     is Opcode_Kind range OP_NOP1       .. OP_NOP10;
  subtype Pseudo_Opcode_Kind       is Opcode_Kind range OP_PUBKEYHASH .. OP_INVALIDOPCODE;

  -------------------
  -- Sparse Groups --
  -------------------
  subtype Disabled_Opcode_Kind is Opcode_Kind with Static_Predicate => Disabled_Opcode_Kind in
    OP_CAT    | OP_SUBSTR | OP_LEFT | OP_RIGHT |
    OP_INVERT | OP_AND    | OP_OR   | OP_XOR   |
    OP_2DIV   | OP_NEGATE | OP_MUL  | OP_DIV   |
    OP_MOD    | OP_LSHIFT | OP_RSHIFT;
  subtype Reserved_Opcode_Kind is Opcode_Kind with Static_Predicate => Reserved_Opcode_Kind in
    OP_RESERVED | OP_VER | OP_VERIF | OP_VERNOTIF | OP_RESERVED1 | OP_RESERVED2;
  subtype Ignored_Opcode_Kind is Opcode_Kind with Static_Predicate => Ignored_Opcode_Kind in
    OP_NOP1 | OP_NOP4 | OP_NOP5 | OP_NOP6  | OP_NOP7 | OP_NOP8 | OP_NOP9 | OP_NOP10;

  -----------
  -- Types --
  -----------
  subtype Data_Count_Range is Byte range 16#01# .. 16#4B#;
  type Opcode_Kind_Array is array (Positive range <>) of Opcode_Kind;

  -----------------
  -- Subprograms --
  -----------------
  function To_Opcode_Kind is new Ada.Unchecked_Conversion (Source => Byte,        Target => Opcode_Kind);
  function To_Byte        is new Ada.Unchecked_Conversion (Source => Opcode_Kind, Target => Byte);
  function To_Byte_Array (Script : in Opcode_Kind_Array) return Byte_Array;

  procedure Evaluate (Script : in Byte_Array);
  procedure Evaluate (
    Script          : in  Byte_Array;
    Primary_Stack   : out Byte_Array_Stacks.Stack_Type;
    Secondary_Stack : out Byte_Array_Stacks.Stack_Type);

  ----------------
  -- Exceptions --
  ----------------
  Invalid_Opcode        : exception;
  Disabled_Opcode       : exception;
  Reserved_Opcode       : exception;
  Unexpected_Opcode     : exception;
  Verification_Failed   : exception;
  Op_Return_Encountered : exception;
  Unimplemented_Feature : exception;

-------
private
-------

  generic
    Script : Byte_Array;
  package Parser is
    function  At_EOS  return Boolean;
    procedure Skip;
    function  Peek    return Byte;
    function  Peek    return Opcode_Kind is (To_Opcode_Kind (Peek));
    function  Current return Byte;
    function  Current return Opcode_Kind is (To_Opcode_Kind (Current));
    function  Next    return Byte;
    function  Next    return Opcode_Kind is (To_Opcode_Kind (Next));
    function  Assert     (Opcodes : in Opcode_Kind_Array) return Boolean;
    function  Assert     (Opcode  : in Opcode_Kind)       return Boolean is (Assert ((1 => Opcode)));
    procedure Skip_Until (Opcodes : in Opcode_Kind_Array);
    procedure Skip_Until (Opcode  : in Opcode_Kind);
    procedure Ensure     (Opcodes : in Opcode_Kind_Array);
    procedure Ensure     (Opcode  : in Opcode_Kind);
    procedure Skip_If_Block;
    procedure Skip_Else_Block;
    procedure Skip_If_Else_Block;
  end;

end;
