
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Script;                use Bitcoin.Script;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Script.Stack_Tests is
  use Byte_Array_Stacks;

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_OP_TOALTSTACK'Access,   "OP_TOALTSTACK");
    Register_Routine (T, Test_OP_FROMALTSTACK'Access, "OP_FROMALTSTACK");
    Register_Routine (T, Test_OP_2DROP'Access,        "OP_2DROP");
    Register_Routine (T, Test_OP_2DUP'Access,         "OP_2DUP");
    Register_Routine (T, Test_OP_3DUP'Access,         "OP_3DUP");
    Register_Routine (T, Test_OP_2OVER'Access,        "OP_2OVER");
    Register_Routine (T, Test_OP_2ROT'Access,         "OP_2ROT");
    Register_Routine (T, Test_OP_2SWAP'Access,        "OP_2SWAP");
    Register_Routine (T, Test_OP_IFDUP'Access,        "OP_IFDUP");
    Register_Routine (T, Test_OP_DEPTH'Access,        "OP_DEPTH");
    Register_Routine (T, Test_OP_DROP'Access,         "OP_DROP");
    Register_Routine (T, Test_OP_DUP'Access,          "OP_DUP");
    Register_Routine (T, Test_OP_NIP'Access,          "OP_NIP");
    Register_Routine (T, Test_OP_OVER'Access,         "OP_OVER");
    Register_Routine (T, Test_OP_PICK'Access,         "OP_PICK");
    Register_Routine (T, Test_OP_ROLL'Access,         "OP_ROLL");
    Register_Routine (T, Test_OP_ROT'Access,          "OP_ROT");
    Register_Routine (T, Test_OP_SWAP'Access,         "OP_SWAP");
    Register_Routine (T, Test_OP_TUCK'Access,         "OP_TUCK");
  end Register_Tests;

  ---------------
  -- Constants --
  ---------------
    DATA_1 : constant Byte_Array := (1 .. 3 => 16#00#, 4 => 16#01#);
    DATA_2 : constant Byte_Array := (1 .. 3 => 16#00#, 4 => 16#02#);
    DATA_3 : constant Byte_Array := (1 .. 3 => 16#00#, 4 => 16#03#);
    DATA_4 : constant Byte_Array := (1 .. 3 => 16#00#, 4 => 16#04#);
    DATA_5 : constant Byte_Array := (1 .. 3 => 16#00#, 4 => 16#05#);
    DATA_6 : constant Byte_Array := (1 .. 3 => 16#00#, 4 => 16#06#);

  ------------------------
  -- Test_OP_TOALTSTACK --
  ------------------------
  procedure Test_OP_TOALTSTACK (Test : in out Test_Cases.Test_Case'Class) is
    DATA            : constant Byte_Array := (1 => 16#AA#, 2 => 16#55#);
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (1 => OP_TOALTSTACK));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Push (Primary_Stack, DATA);
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Naturals_Equal    (Expected => 0,    Actual => Size (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Peek (Secondary_Stack));
  end;

  --------------------------
  -- Test_OP_FROMALTSTACK --
  --------------------------
  procedure Test_OP_FROMALTSTACK (Test : in out Test_Cases.Test_Case'Class) is
    DATA            : constant Byte_Array := (1 => 16#AA#, 2 => 16#55#);
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (1 => OP_FROMALTSTACK));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Push (Secondary_Stack, DATA);
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA, Actual => Peek (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,    Actual => Size (Secondary_Stack));
  end;

  -------------------
  -- Test_OP_2DROP --
  -------------------
  procedure Test_OP_2DROP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array (1 .. 4) := To_Byte_Array (Script => (OP_1, OP_2, OP_3, OP_2DROP));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Booleans_Equal (Expected => True, Actual => Is_One (Peek (Primary_Stack)));
    Assert_Naturals_Equal (Expected => 0,    Actual => Size (Secondary_Stack));
  end;

  ------------------
  -- Test_OP_2DUP --
  ------------------
  procedure Test_OP_2DUP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (OP_1, OP_2, OP_3, OP_2DUP));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA_3, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_3, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_1, Actual => Pop (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Secondary_Stack));
  end;

  ------------------
  -- Test_OP_3DUP --
  ------------------
  procedure Test_OP_3DUP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (OP_1, OP_2, OP_3, OP_3DUP));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA_3, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_1, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_3, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_1, Actual => Pop (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Secondary_Stack));
  end;

  -------------------
  -- Test_OP_2OVER --
  -------------------
  procedure Test_OP_2OVER (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (OP_1, OP_2, OP_3, OP_2OVER));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_1, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_3, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_1, Actual => Pop (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Secondary_Stack));
  end;

  ------------------
  -- Test_OP_2ROT --
  ------------------
  procedure Test_OP_2ROT (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (OP_1, OP_2, OP_3, OP_4, OP_5, OP_6, OP_2ROT));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_1, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_6, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_5, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_4, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_3, Actual => Pop (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Secondary_Stack));
  end;

  -------------------
  -- Test_OP_2SWAP --
  -------------------
  procedure Test_OP_2SWAP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := To_Byte_Array (Script => (OP_1, OP_2, OP_3, OP_4, OP_2SWAP));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Evaluate (SCRIPT, Primary_Stack, Secondary_Stack);
    Assert_Byte_Arrays_Equal (Expected => DATA_2, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_1, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_4, Actual => Pop (Primary_Stack));
    Assert_Byte_Arrays_Equal (Expected => DATA_3, Actual => Pop (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Primary_Stack));
    Assert_Naturals_Equal    (Expected => 0,      Actual => Size (Secondary_Stack));
  end;

  -------------------
  -- Test_OP_IFDUP --
  -------------------
  procedure Test_OP_IFDUP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  -------------------
  -- Test_OP_DEPTH --
  -------------------
  procedure Test_OP_DEPTH (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_DROP --
  ------------------
  procedure Test_OP_DROP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  -----------------
  -- Test_OP_DUP --
  -----------------
  procedure Test_OP_DUP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  -----------------
  -- Test_OP_NIP --
  -----------------
  procedure Test_OP_NIP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_OVER --
  ------------------
  procedure Test_OP_OVER (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_PICK --
  ------------------
  procedure Test_OP_PICK (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_ROLL --
  ------------------
  procedure Test_OP_ROLL (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  -----------------
  -- Test_OP_ROT --
  -----------------
  procedure Test_OP_ROT (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_SWAP --
  ------------------
  procedure Test_OP_SWAP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_TUCK --
  ------------------
  procedure Test_OP_TUCK (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;
end;
