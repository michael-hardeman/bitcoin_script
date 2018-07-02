
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
    Register_Routine (T, Test_OP_0'Access,        "OP_0");
    Register_Routine (T, Test_OP_PUSHDATA'Access, "OP_PUSHDATA");
  end Register_Tests;

  ---------------
  -- Test_OP_0 --
  ---------------
  procedure Test_OP_0 (Test : in out Test_Cases.Test_Case'Class) is
    use Byte_Array_Stacks;

    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);

    Assert_Byte_Arrays_Equal (Expected => (1 .. 4 => 16#00#), Actual => Get (Primary_Stack, 1));
  end;

  ----------------------
  -- Test_OP_PUSHDATA --
  ----------------------
  procedure Test_OP_PUSHDATA (Test : in out Test_Cases.Test_Case'Class) is
    use Byte_Array_Stacks;

    Script          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    for I in 16#01# .. 16#4B# loop
      declare
        DATA            : constant Byte_Array (1 .. Positive (I)) := (others => 16#FF#);
        SCRIPT          : constant Byte_Array                     := (1 => Byte (I)) & Data;
        Primary_Stack   :          Stack_Type;
        Secondary_Stack :          Stack_Type;
      begin
        Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);

        Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Get (Primary_Stack, 1));
      end;
    end loop;
  end;
end;
