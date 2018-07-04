
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Script;                use Bitcoin.Script;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Script.Flow_Control_Tests is
  use Byte_Array_Stacks;

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_OP_NOP'Access,      "OP_NOP");
    Register_Routine (T, Test_OP_VER'Access,      "OP_VER");
    Register_Routine (T, Test_OP_IF'Access,       "OP_IF");
    Register_Routine (T, Test_OP_NOTIF'Access,    "OP_NOTIF");
    Register_Routine (T, Test_OP_VERIF'Access,    "OP_VERIF");
    Register_Routine (T, Test_OP_VERNOTIF'Access, "OP_VERNOTIF");
    Register_Routine (T, Test_OP_ELSE'Access,     "OP_ELSE");
    Register_Routine (T, Test_OP_VERIFY'Access,   "OP_VERIFY");
    Register_Routine (T, Test_OP_RETURN'Access,   "OP_RETURN");
  end Register_Tests;

  -----------------
  -- Test_OP_NOP --
  -----------------
  procedure Test_OP_NOP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_NOP));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Primary_Stack));
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Secondary_Stack));
  end;

  -----------------
  -- Test_OP_VER --
  -----------------
  procedure Evaluate_OP_VER is begin Evaluate ((1 => To_Byte (OP_VER))); end;
  procedure Test_OP_VER (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_NOP));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Assert_Exception (Evaluate_OP_VER'Access, "Expected OP_VER to raise1 an error.");
  end;

  ----------------
  -- Test_OP_IF --
  ----------------
  procedure Test_OP_IF (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT : constant Byte_Array (1 .. 20) := (
      To_Byte (OP_1),
      To_Byte (OP_IF),
      To_Byte (  OP_NOP),
      To_Byte (  OP_NOP),
      To_Byte (OP_ENDIF),
      To_Byte (OP_0),
      To_Byte (OP_IF),
      To_Byte (  OP_RESERVED),
      To_Byte (OP_ENDIF),
      To_Byte (OP_1),
      To_Byte (OP_IF),
      To_Byte (  OP_0),
      To_Byte (  OP_IF),
      To_Byte (    OP_RESERVED),
      To_Byte (  OP_ENDIF),
      To_Byte (  OP_1),
      To_Byte (  OP_IF),
      To_Byte (    OP_NOP),
      To_Byte (  OP_ENDIF),
      To_Byte (OP_ENDIF));
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Primary_Stack));
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Secondary_Stack));
  end;

  -------------------
  -- Test_OP_NOTIF --
  -------------------
  procedure Test_OP_NOTIF (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT : constant Byte_Array (1 .. 20) := (
      To_Byte (OP_1),
      To_Byte (OP_NOTIF),
      To_Byte (  OP_RESERVED),
      To_Byte (  OP_RESERVED),
      To_Byte (OP_ENDIF),
      To_Byte (OP_0),
      To_Byte (OP_NOTIF),
      To_Byte (  OP_NOP),
      To_Byte (OP_ENDIF),
      To_Byte (OP_0),
      To_Byte (OP_NOTIF),
      To_Byte (  OP_1),
      To_Byte (  OP_NOTIF),
      To_Byte (    OP_RESERVED),
      To_Byte (  OP_ENDIF),
      To_Byte (  OP_0),
      To_Byte (  OP_NOTIF),
      To_Byte (    OP_NOP),
      To_Byte (  OP_ENDIF),
      To_Byte (OP_ENDIF));
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Primary_Stack));
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Secondary_Stack));
  end;

  -------------------
  -- Test_OP_VERIF --
  -------------------
  procedure Evaluate_OP_VERIF is begin Evaluate ((1 => To_Byte (OP_VERIF))); end;
  procedure Test_OP_VERIF (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_VERIF'Access, "Expected OP_VERIF to raise an error.");
  end;

  ----------------------
  -- Test_OP_VERNOTIF --
  ----------------------
  procedure Evaluate_OP_VERNOTIF is begin Evaluate ((1 => To_Byte (OP_VERNOTIF))); end;
  procedure Test_OP_VERNOTIF (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Exception (Evaluate_OP_VERNOTIF'Access, "Expected OP_VERNOTIF to raise an error.");
  end;

  ------------------
  -- Test_OP_ELSE --
  ------------------
  procedure Evaluate_OP_ELSE is begin Evaluate ((1 => To_Byte (OP_ELSE))); end;
  procedure Test_OP_ELSE (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT : constant Byte_Array (1 .. 27) := (
      To_Byte (OP_1),
      To_Byte (OP_IF),
      To_Byte (  OP_NOP),
      To_Byte (OP_ELSE),
      To_Byte (  OP_RESERVED),
      To_Byte (OP_ENDIF),
      To_Byte (OP_0),
      To_Byte (OP_IF),
      To_Byte (  OP_RESERVED),
      To_Byte (OP_ELSE),
      To_Byte (  OP_NOP),
      To_Byte (OP_ENDIF),
      To_Byte (OP_1),
      To_Byte (OP_IF),
      To_Byte (  OP_0),
      To_Byte (  OP_IF),
      To_Byte (    OP_RESERVED),
      To_Byte (  OP_ELSE),
      To_Byte (    OP_NOP),
      To_Byte (  OP_ENDIF),
      To_Byte (  OP_1),
      To_Byte (  OP_IF),
      To_Byte (    OP_NOP),
      To_Byte (  OP_Else),
      To_Byte (    OP_RESERVED),
      To_Byte (  OP_ENDIF),
      To_Byte (OP_ENDIF));
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Assert_Exception (Evaluate_OP_VERNOTIF'Access, "Expected OP_ELSE without an OP_IF to raise an error.");

    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Primary_Stack));
    Assert_Naturals_Equal (Expected => 0, Actual => Size (Secondary_Stack));
  end;

  --------------------
  -- Test_OP_VERIFY --
  --------------------
  procedure Test_OP_VERIFY (Test : in out Test_Cases.Test_Case'Class) is
  begin
    raise Program_Error;
  end;

  --------------------
  -- Test_OP_RETURN --
  --------------------
  procedure Test_OP_RETURN (Test : in out Test_Cases.Test_Case'Class) is
  begin
    raise Program_Error;
  end;
end;
