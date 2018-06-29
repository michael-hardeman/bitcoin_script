
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin.Data.Stacks;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Data.Stacks_Tests is

  package String_Stacks is new Bitcoin.Data.Stacks (Positive, String, "=");
  use String_Stacks;

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_Top_Index'Access,   "Ensure Top_Index returns the array index of the last item.");
    Register_Routine (T, Test_Size'Access,        "Ensure Size returns the stack size.");
    Register_Routine (T, Test_Get'Access,         "Ensure Get returns the item at the array index.");
    Register_Routine (T, Test_Peek'Access,        "Ensure Peek returns the top item on the stack without modifying it.");
    Register_Routine (T, Test_Push'Access,        "Ensure Push adds the new item to the top.");
    Register_Routine (T, Test_Pop'Access,         "Ensure Pop removes and returns the top item.");
    Register_Routine (T, Test_Pop_Ignored'Access, "Ensure Pop removes the top item.");
    Register_Routine (T, Test_Swap'Access,        "Ensure Swap swaps two items on the stack by index.");
    Register_Routine (T, Test_Delate'Access,      "Ensure Delete removes the item at the provided index.");
  end Register_Tests;

  --------------------
  -- Test_Top_Index --
  --------------------
  procedure Test_Top_Index (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  ---------------
  -- Test_Size --
  ---------------
  procedure Test_Size (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  --------------
  -- Test_Get --
  --------------
  procedure Test_Get (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  ---------------
  -- Test_Peek --
  ---------------
  procedure Test_Peek (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  ---------------
  -- Test_Push --
  ---------------
  procedure Test_Push (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  --------------
  -- Test_Pop --
  --------------
  procedure Test_Pop (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  ----------------------
  -- Test_Pop_Ignored --
  ----------------------
  procedure Test_Pop_Ignored (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  ---------------
  -- Test_Swap --
  ---------------
  procedure Test_Swap (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

  -----------------
  -- Test_Delate --
  -----------------
  procedure Test_Delate (Test : in out Test_Cases.Test_Case'Class) is
    Stack : Stack_Type;
  begin
    null;
  end;

end;
