
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Script.Stack_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  function Name (T : TC) return Message_String is (Aunit.Format ("Bitcoin.Script (Stack Opcodes)"));

  overriding procedure Register_Tests (T : in out TC);

  -----------
  -- Stack --
  -----------
  -- OP_TOALTSTACK, OP_FROMALTSTACK, OP_2DROP, OP_2DUP,  OP_3DUP,
  -- OP_2OVER,      OP_2ROT,         OP_2SWAP, OP_IFDUP, OP_DEPTH,
  -- OP_DROP,       OP_DUP,          OP_NIP,   OP_OVER,  OP_PICK,
  -- OP_ROLL,       OP_ROT,          OP_SWAP,  OP_TUCK,

  procedure Test_OP_TOALTSTACK   (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_FROMALTSTACK (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_2DROP        (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_2DUP         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_3DUP         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_2OVER        (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_2ROT         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_2SWAP        (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_IFDUP        (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_DEPTH        (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_DROP         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_DUP          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_NIP          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_OVER         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_PICK         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_ROLL         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_ROT          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_SWAP         (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_TUCK         (Test : in out Test_Cases.Test_Case'Class);

end;
