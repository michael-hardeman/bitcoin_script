
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Script;                use Bitcoin.Script;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Script.Op_Code_Tests is

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_OP_0'Access, "OP_0");
  end Register_Tests;

  ---------------
  -- Test_OP_0 --
  ---------------
  procedure Test_OP_0 (Test : in out Test_Cases.Test_Case'Class) is
    Script          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   : Byte_Array_Stacks.Stack_Type;
    Secondary_Stack : Byte_Array_Stacks.Stack_Type;
  begin
    Evaluate (Script, Primary_Stack, Secondary_Stack);
  end;
end;
