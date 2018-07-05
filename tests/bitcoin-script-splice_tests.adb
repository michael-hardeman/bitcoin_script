
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Script;                use Bitcoin.Script;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Script.Splice_Tests is
  use Byte_Array_Stacks;

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_OP_CAT'Access,    "OP_CAT");
    Register_Routine (T, Test_OP_SUBSTR'Access, "OP_SUBSTR");
    Register_Routine (T, Test_OP_LEFT'Access,   "OP_LEFT");
    Register_Routine (T, Test_OP_RIGHT'Access,  "OP_RIGHT");
    Register_Routine (T, Test_OP_SIZE'Access,   "OP_SIZE");
  end Register_Tests;

  -----------------
  -- Test_OP_CAT --
  -----------------
  procedure Evaluate_OP_CAT is begin Evaluate ((1 => To_Byte (OP_CAT))); end;
  procedure Test_OP_CAT (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_CAT'Access, "Expected OP_CAT to raise an error.");
  end;

  --------------------
  -- Test_OP_SUBSTR --
  --------------------
  procedure Evaluate_OP_SUBSTR is begin Evaluate ((1 => To_Byte (OP_SUBSTR))); end;
  procedure Test_OP_SUBSTR (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_SUBSTR'Access, "Expected OP_SUBSTR to raise an error.");
  end;

  ------------------
  -- Test_OP_LEFT --
  ------------------
  procedure Evaluate_OP_LEFT is begin Evaluate ((1 => To_Byte (OP_LEFT))); end;
  procedure Test_OP_LEFT (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_LEFT'Access, "Expected OP_LEFT to raise an error.");
  end;

  -------------------
  -- Test_OP_RIGHT --
  -------------------
  procedure Evaluate_OP_RIGHT is begin Evaluate ((1 => To_Byte (OP_RIGHT))); end;
  procedure Test_OP_RIGHT (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_RIGHT'Access, "Expected OP_RIGHT to raise an error.");
  end;

  ------------------
  -- Test_OP_SIZE --
  ------------------
  procedure Test_OP_SIZE (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (OP_1, OP_SIZE));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => (1 => 4), Actual => Pop  (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 1,        Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,        Actual => Size (Secondary_Stack));
  end;
end;
