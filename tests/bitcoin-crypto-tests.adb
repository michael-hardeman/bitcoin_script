
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Crypto;                use Bitcoin.Crypto;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Crypto.Tests is

  ---------------
  -- Constants --
  ---------------
  TEST_PRIVATE_KEY : Byte_Array := (
    16#16#, 16#26#, 16#07#, 16#83#, 16#e4#, 16#0b#, 16#16#, 16#73#,
    16#16#, 16#73#, 16#62#, 16#2a#, 16#c8#, 16#a5#, 16#b0#, 16#45#,
    16#fc#, 16#3e#, 16#a4#, 16#af#, 16#70#, 16#f7#, 16#27#, 16#f3#,
    16#f9#, 16#e9#, 16#2b#, 16#dd#, 16#3a#, 16#1d#, 16#dc#, 16#42#);

  TEST_MESSAGE : Byte_Array := To_Byte_Array ("I approve this message.");

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_Generate'Access,        "Generate");
    Register_Routine (T, From_Private_Key'Access,     "From_Private_Key");
    Register_Routine (T, Test_Sign_And_Verify'Access, "Sign And Verify");
  end Register_Tests;

  -------------------
  -- Test_Generate --
  -------------------
  procedure Test_Generate (Test : in out Test_Cases.Test_Case'Class) is
    Key_Pair : Key_Pair_Type;
  begin
    Generate (Key_Pair, secp256k1);
    declare
      Public_Key  : Byte_Array := Get_Public_Key  (Key_Pair, Compressed);
      Private_Key : Byte_Array := Get_Private_Key (Key_Pair);
    begin
      Assert_Booleans_Equal (
        Expected => TRUE,
        Actual   => Public_Key'Length > 0);

      Assert_Booleans_Equal (
        Expected => TRUE,
        Actual   => Private_Key'Length > 0);
    end;
  end;

  ----------------------
  -- From_Private_Key --
  ----------------------
  procedure From_Private_Key (Test : in out Test_Cases.Test_Case'Class) is
    Key_Pair : Key_Pair_Type;
  begin
    From_Private_Key (Key_Pair, secp256k1, TEST_PRIVATE_KEY);

    declare
      Public_Key  : Byte_Array := Get_Public_Key  (Key_Pair, Compressed);
      Private_Key : Byte_Array := Get_Private_Key (Key_Pair);
    begin
      Assert_Booleans_Equal (
        Expected => TRUE,
        Actual   => Public_Key'Length > 0);

      Assert_Byte_Arrays_Equal (
        Expected => TEST_PRIVATE_KEY,
        Actual   => Private_Key);
    end;
  end;

  --------------------------
  -- Test_Sign_And_Verify --
  --------------------------
  procedure Test_Sign_And_Verify (Test : in out Test_Cases.Test_Case'Class) is
    Key_Pair : Key_Pair_Type;
  begin
    From_Private_Key (Key_Pair, secp256k1, TEST_PRIVATE_KEY);

    declare
      Signature : Byte_Array := Sign (Key_Pair, TEST_MESSAGE);
    begin
      Assert_Booleans_Equal (
        Expected => TRUE,
        Actual   => Verify (Key_Pair, Signature, TEST_MESSAGE));
    end;
  end;

end;
