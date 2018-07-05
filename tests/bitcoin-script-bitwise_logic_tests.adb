
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Script;                use Bitcoin.Script;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Script.Bitwise_Logic_Tests is
  use Byte_Array_Stacks;

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_OP_INVERT'Access,      "OP_INVERT");
    Register_Routine (T, Test_OP_AND'Access,         "OP_AND");
    Register_Routine (T, Test_OP_OR'Access,          "OP_OR");
    Register_Routine (T, Test_OP_XOR'Access,         "OP_XOR");
    Register_Routine (T, Test_OP_EQUAL'Access,       "OP_EQUAL");
    Register_Routine (T, Test_OP_EQUALVERIFY'Access, "OP_EQUALVERIFY");
    Register_Routine (T, Test_OP_RESERVED1'Access,   "OP_RESERVED1");
    Register_Routine (T, Test_OP_RESERVED2'Access,   "OP_RESERVED2");
  end Register_Tests;

  --------------------
  -- Test_OP_INVERT --
  --------------------
  procedure Evaluate_OP_INVERT is begin Evaluate ((1 => To_Byte (OP_INVERT))); end;
  procedure Test_OP_INVERT (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_INVERT'Access, "Expected OP_INVERT to raise an error.");
  end;

  -----------------
  -- Test_OP_AND --
  -----------------
  procedure Evaluate_OP_AND is begin Evaluate ((1 => To_Byte (OP_AND))); end;
  procedure Test_OP_AND (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_AND'Access, "Expected OP_AND to raise an error.");
  end;

  ----------------
  -- Test_OP_OR --
  ----------------
  procedure Evaluate_OP_OR is begin Evaluate ((1 => To_Byte (OP_OR))); end;
  procedure Test_OP_OR (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_OR'Access, "Expected OP_OR to raise an error.");
  end;

  -----------------
  -- Test_OP_XOR --
  -----------------
  procedure Evaluate_OP_XOR is begin Evaluate ((1 => To_Byte (OP_XOR))); end;
  procedure Test_OP_XOR (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_XOR'Access, "Expected OP_XOR to raise an error.");
  end;

  -------------------
  -- Test_OP_EQUAL --
  -------------------
  procedure Test_OP_EQUAL (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (
      OP_1, OP_1, OP_EQUAL,
      OP_0, OP_1, OP_EQUAL));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => (1 .. 4 => 16#00#),              Actual => Pop  (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => (1 .. 3 => 16#00#, 4 => 16#01#), Actual => Pop  (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,                               Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,                               Actual => Size (Primary_Stack));
  end;

  -------------------------
  -- Test_OP_EQUALVERIFY --
  -------------------------
  procedure Evaluate_OP_EQUALVERIFY is begin Evaluate (To_Byte_Array (Script => (OP_0, OP_1, OP_EQUALVERIFY))); end;
  procedure Test_OP_EQUALVERIFY (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (OP_1, OP_1, OP_EQUALVERIFY));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Naturals_Equal    (Expected => 0, Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0, Actual => Size (Primary_Stack));

    -- Assert_Exception (Evaluate_OP_EQUALVERIFY'Access, "Expected OP_EQUALVERIFY to raise an error.");
  end;

  -----------------------
  -- Test_OP_RESERVED1 --
  -----------------------
  procedure Evaluate_OP_RESERVED1 is begin Evaluate ((1 => To_Byte (OP_RESERVED1))); end;
  procedure Test_OP_RESERVED1 (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_RESERVED1'Access, "Expected OP_RESERVED1 to raise an error.");
  end;

  -----------------------
  -- Test_OP_RESERVED1 --
  -----------------------
  procedure Evaluate_OP_RESERVED2 is begin Evaluate ((1 => To_Byte (OP_RESERVED2))); end;
  procedure Test_OP_RESERVED2 (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_RESERVED2'Access, "Expected OP_RESERVED2 to raise an error.");
  end;
end;
