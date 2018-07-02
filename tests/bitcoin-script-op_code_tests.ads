
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Script.Op_Code_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  function Name (T : TC) return Message_String is (Aunit.Format ("Bitcoin.Script Op_Code"));

  overriding procedure Register_Tests (T : in out TC);

  ---------------
  -- Constants --
  ---------------
  procedure Test_OP_0         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_PUSHDATA  (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_PUSHDATA1 (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_PUSHDATA2 (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_PUSHDATA4 (Test : in out Test_Cases.Test_Case'Class);
  -- OP_0,         OP_PUSHDATA1, OP_PUSHDATA2,
  -- OP_PUSHDATA4, OP_1NEGATE,   OP_RESERVED,
  -- OP_1,         OP_2,         OP_3,
  -- OP_4,         OP_5,         OP_6,
  -- OP_7,         OP_8,         OP_9,
  -- OP_10,        OP_11,        OP_12,
  -- OP_13,        OP_14,        OP_15,
  -- OP_16,

  ------------------
  -- Flow Control --
  ------------------
  -- OP_NOP,    OP_VER,
  -- OP_IF,     OP_NOTIF,
  -- OP_VERIF,  OP_VERNOTIF,
  -- OP_ELSE,   OP_ENDIF,
  -- OP_VERIFY, OP_RETURN,

  -----------
  -- Stack --
  -----------
  -- OP_TOALTSTACK, OP_FROMALTSTACK, OP_2DROP, OP_2DUP,  OP_3DUP,
  -- OP_2OVER,      OP_2ROT,         OP_2SWAP, OP_IFDUP, OP_DEPTH,
  -- OP_DROP,       OP_DUP,          OP_NIP,   OP_OVER,  OP_PICK,
  -- OP_ROLL,       OP_ROT,          OP_SWAP,  OP_TUCK,

  ------------
  -- Splice --
  ------------
  -- OP_CAT,   OP_SUBSTR, OP_LEFT,
  -- OP_RIGHT, OP_SIZE,

  -------------------
  -- Bitwise Logic --
  -------------------
  -- OP_INVERT, OP_AND,         OP_OR,        OP_XOR,
  -- OP_EQUAL,  OP_EQUALVERIFY, OP_RESERVED1, OP_RESERVED2,

  ----------------
  -- Arithmetic --
  ----------------
  -- OP_1ADD,           OP_1SUB,            OP_2MUL,
  -- OP_2DIV,           OP_NEGATE,          OP_ABS,
  -- OP_NOT,            OP_0NOTEQUAL,       OP_ADD,
  -- OP_SUB,            OP_MUL,             OP_DIV,
  -- OP_MOD,            OP_LSHIFT,          OP_RSHIFT,
  -- OP_BOOLAND,        OP_BOOLOR,          OP_NUMEQUAL,
  -- OP_NUMEQUALVERIFY, OP_NUMNOTEQUAL,     OP_LESSTHAN,
  -- OP_GREATERTHAN,    OP_LESSTHANOREQUAL, OP_GREATERTHANOREQUAL,
  -- OP_MIN,            OP_MAX,             OP_WITHIN,

  ------------
  -- Crypto --
  ------------
  -- OP_RIPEMD160,     OP_SHA1,     OP_SHA256,         OP_HASH160,       OP_HASH256,
  -- OP_CODESEPARATOR, OP_CHECKSIG, OP_CHECKSIGVERIFY, OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY,

  --------------
  -- Locktime --
  --------------
  -- OP_NOP1, OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY, OP_NOP4, OP_NOP5,
  -- OP_NOP6, OP_NOP7,                OP_NOP8,                OP_NOP9, OP_NOP10,

  ------------------
  -- Pseudo-words --
  ------------------
  -- OP_PUBKEYHASH, OP_PUBKEY, OP_INVALIDOPCODE
end;
