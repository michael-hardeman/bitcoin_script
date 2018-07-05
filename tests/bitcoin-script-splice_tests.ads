
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Script.Splice_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  function Name (T : TC) return Message_String is (Aunit.Format ("Bitcoin.Script (Splice Opcodes)"));

  overriding procedure Register_Tests (T : in out TC);

  ------------
  -- Splice --
  ------------
  -- OP_CAT,   OP_SUBSTR, OP_LEFT,
  -- OP_RIGHT, OP_SIZE,

  procedure Test_OP_CAT    (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_SUBSTR (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_LEFT   (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_RIGHT  (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_SIZE   (Test : in out Test_Cases.Test_Case'Class);

end;
