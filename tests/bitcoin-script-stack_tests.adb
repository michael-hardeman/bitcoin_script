
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

  ------------------------
  -- Test_OP_TOALTSTACK --
  ------------------------
  procedure Test_OP_TOALTSTACK (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_TOALTSTACK));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    Push (Primary_Stack, (1 => 16#55#));

  end;

  --------------------------
  -- Test_OP_FROMALTSTACK --
  --------------------------
  procedure Test_OP_FROMALTSTACK (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  -------------------
  -- Test_OP_2DROP --
  -------------------
  procedure Test_OP_2DROP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_2DUP --
  ------------------
  procedure Test_OP_2DUP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_3DUP --
  ------------------
  procedure Test_OP_3DUP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  -------------------
  -- Test_OP_2OVER --
  -------------------
  procedure Test_OP_2OVER (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  ------------------
  -- Test_OP_2ROT --
  ------------------
  procedure Test_OP_2ROT (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
  end;

  -------------------
  -- Test_OP_2SWAP --
  -------------------
  procedure Test_OP_2SWAP (Test : in out Test_Cases.Test_Case'Class) is
    SCRIPT          : constant Byte_Array := (1 => To_Byte (OP_0));
    Primary_Stack   :          Stack_Type;
    Secondary_Stack :          Stack_Type;
  begin
    raise Program_Error;
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
