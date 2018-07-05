
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
    Register_Routine (T, Test_OP_1NEGATE'Access,   "OP_1NEGATE");
    Register_Routine (T, Test_OP_RESERVED'Access,  "OP_RESERVED");
    Register_Routine (T, Test_OP_1_To_16'Access,   "OP_1 .. OP_16");
  end Register_Tests;

  ---------------
  -- Test_OP_0 --
  ---------------
  procedure Test_OP_0 (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (1 => OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => (1 .. 4 => 16#00#), Actual => Peek (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,                  Actual => Size (Secondary_Stack));
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
        Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Peek (Primary_Stack));
        Assert_Naturals_Equal    (Expected => 0,    Actual => Size (Secondary_Stack));
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
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Peek (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,    Actual => Size (Secondary_Stack));
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
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Peek (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,    Actual => Size (Secondary_Stack));
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
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Peek (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,    Actual => Size (Secondary_Stack));
  end;

  ---------------------
  -- Test_OP_1NEGATE --
  ---------------------
  procedure Test_OP_1NEGATE (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_1NEGATE));
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => (1 .. 4 => 16#FF#), Actual => Peek (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,                  Actual => Size (Secondary_Stack));
  end;

  procedure Evaluate_OP_RESERVED is begin Evaluate ((1 => To_Byte (OP_RESERVED))); end;

  ----------------------
  -- Test_OP_RESERVED --
  ----------------------
  procedure Test_OP_RESERVED (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Exception (Evaluate_OP_RESERVED'Access, "Expected evaluating OP_RESERVED to throw an error.");
  end;

  ---------------------
  -- Test_OP_1_To_16 --
  ---------------------
  procedure Test_OP_1_To_16 (Test : in out Test_Cases.Test_Case'Class) is
  begin
    for I in 16#00# .. 16#0F# loop
      declare
        SCRIPT          : constant Byte_Array := (1 => (To_Byte (OP_1) + Byte (I)));
        Primary_Stack   :          Stack_Type;
        Secondary_Stack :          Stack_Type;
      begin
        Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
        Assert_Byte_Arrays_Equal (Expected => (1 .. 3 => 16#00#, 4 => Byte (I + 1)), Actual => Peek (Primary_Stack));
        Assert_Naturals_Equal    (Expected => 0,                                     Actual => Size (Secondary_Stack));
      end;
    end loop;
  end;
end;
