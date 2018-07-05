with Bitcoin.Script.Constant_Tests;
with Bitcoin.Script.Flow_Control_Tests;
with Bitcoin.Script.Stack_Tests;
with Bitcoin.Script.Splice_Tests;
with Bitcoin.Script.Bitwise_Logic_Tests;
-- with Bitcoin.Script.Arithmetic_Tests;
-- with Bitcoin.Script.Crypto_Tests;
-- with Bitcoin.Script.Lock_Time_Tests;
-- with Bitcoin.Script.Pseudo_Words_Tests;
with Bitcoin.Script.Tests;

package body Bitcoin.Script.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Constant_Test_Case      : aliased Bitcoin.Script.Constant_Tests.TC;
  Flow_Control_Test_Case  : aliased Bitcoin.Script.Flow_Control_Tests.TC;
  Stack_Test_Case         : aliased Bitcoin.Script.Stack_Tests.TC;
  Splice_Test_Case        : aliased Bitcoin.Script.Splice_Tests.TC;
  Bitwise_Logic_Test_Case : aliased Bitcoin.Script.Bitwise_Logic_Tests.TC;
  -- Arithmetic_Test_Case    : aliased Bitcoin.Script.Arithmetic_Tests.TC;
  -- Crypto_Test_Case        : aliased Bitcoin.Script.Crypto_Tests.TC;
  -- Lock_Time_Test_Case     : aliased Bitcoin.Script.Lock_Time_Tests.TC;
  -- Pseudo_Words_Test_Case  : aliased Bitcoin.Script.Pseudo_Words_Tests.TC;
  Script_Test_Case        : aliased Bitcoin.Script.Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Constant_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, Flow_Control_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, Stack_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, Splice_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, Bitwise_Logic_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Arithmetic_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Crypto_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Lock_Time_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Pseudo_Words_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, Script_Test_Case'Access);
    return Result'Access;
  end Suite;

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
