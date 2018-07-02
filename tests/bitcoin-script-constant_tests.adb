
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Script;                use Bitcoin.Script;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Script.Constant_Tests is
  use Byte_Array_Stacks;

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_OP_0'Access,         "OP_0");
    Register_Routine (T, Test_OP_PUSHDATA'Access,  "OP_PUSHDATA");
    Register_Routine (T, Test_OP_PUSHDATA1'Access, "OP_PUSHDATA1");
    Register_Routine (T, Test_OP_PUSHDATA2'Access, "OP_PUSHDATA2");
    Register_Routine (T, Test_OP_PUSHDATA4'Access, "OP_PUSHDATA4");
  end Register_Tests;

  ---------------
  -- Test_OP_0 --
  ---------------
  procedure Test_OP_0 (Test : in out Test_Cases.Test_Case'Class) is
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
  procedure Test_OP_PUSHDATA (Test : in out Test_Cases.Test_Case'Class) is begin
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

  -----------------------
  -- Test_OP_PUSHDATA1 --
  -----------------------
  procedure Test_OP_PUSHDATA1 (Test : in out Test_Cases.Test_Case'Class) is
    LENGTH          : constant Byte       := 16#07#;
    DATA            : constant Byte_Array := (1 .. Positive (LENGTH) => 16#FF#);
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_PUSHDATA1), 2 => LENGTH) & DATA;
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Get (Primary_Stack, 1));
  end;

  -----------------------
  -- Test_OP_PUSHDATA2 --
  -----------------------
  procedure Test_OP_PUSHDATA2 (Test : in out Test_Cases.Test_Case'Class) is
    LENGTH          : constant Byte_Array := (1 => 16#00#, 2 => 16#07#);
    DATA            : constant Byte_Array := (1 .. 7 => 16#FF#);
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_PUSHDATA2)) & LENGTH & DATA;
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Get (Primary_Stack, 1));
  end;

  -----------------------
  -- Test_OP_PUSHDATA4 --
  -----------------------
  procedure Test_OP_PUSHDATA4 (Test : in out Test_Cases.Test_Case'Class) is
    LENGTH          : constant Byte_Array := (1 .. 3 => 16#00#, 4 => 16#07#);
    DATA            : constant Byte_Array := (1 .. 7 => 16#FF#);
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_PUSHDATA4)) & LENGTH & DATA;
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Get (Primary_Stack, 1));
  end;
end;
