
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin.Data.Stacks;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Data.Stacks_Tests is

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_Peek'Access, "Ensure Peek returns a string visualizing the byte array");
    Register_Routine (T, Test_Push'Access, "Ensure Bitcoin.To_Byte_Array converts a string into a byte array of Character'Pos codes");
    Register_Routine (T, Test_Pop'Access,  "Ensure Bitcoin.To_String converts a byte array directly into a String by getting Character'Val");
  end Register_Tests;

  ----------
  -- Name --
  ----------
  function Name (T : TC) return Message_String is
    pragma Unreferenced (T);
  begin
    return AUnit.Format ("Testing Bitcoin.Data.Stacks operations");
  end Name;

  ---------------
  -- Test_Peek --
  ---------------
  procedure Test_Peek (Test : in out Test_Cases.Test_Case'Class) is begin
    null;
  end;

  ---------------
  -- Test_Push --
  ---------------
  procedure Test_Push (Test : in out Test_Cases.Test_Case'Class) is begin
    null;
  end;

  --------------
  -- Test_Pop --
  --------------
  procedure Test_Pop (Test : in out Test_Cases.Test_Case'Class) is begin
    null;
  end;

end;
