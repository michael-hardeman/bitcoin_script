
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Script.Constant_Tests is

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

end;
