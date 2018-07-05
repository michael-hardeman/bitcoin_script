
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Script.Bitwise_Logic_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  function Name (T : TC) return Message_String is (Aunit.Format ("Bitcoin.Script (Stack Opcodes)"));

  overriding procedure Register_Tests (T : in out TC);

  -------------------
  -- Bitwise Logic --
  -------------------
  -- OP_INVERT, OP_AND,         OP_OR,        OP_XOR,
  -- OP_EQUAL,  OP_EQUALVERIFY, OP_RESERVED1, OP_RESERVED2,

  procedure Test_OP_INVERT      (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_AND         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_OR          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_XOR         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_EQUAL       (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_EQUALVERIFY (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_RESERVED1   (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_RESERVED2   (Test : in out Test_Cases.Test_Case'Class);

end;
