
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Script.Flow_Control_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  function Name (T : TC) return Message_String is (Aunit.Format ("Bitcoin.Script (Flow Control Opcodes)"));

  overriding procedure Register_Tests (T : in out TC);

  ------------------
  -- Flow Control --
  ------------------
  -- OP_NOP,    OP_VER,
  -- OP_IF,     OP_NOTIF,
  -- OP_VERIF,  OP_VERNOTIF,
  -- OP_ELSE,   OP_ENDIF,
  -- OP_VERIFY, OP_RETURN,

  procedure Test_OP_NOP       (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_VER       (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_IF        (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_NOTIF     (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_VERIF     (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_VERNOTIF  (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_ELSE      (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_VERIFY    (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_OP_RETURN    (Test : in out Test_Cases.Test_Case'Class);

end;
