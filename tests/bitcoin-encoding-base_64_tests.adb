
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Encoding.Base64;       use Bitcoin.Encoding.Base64;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Encoding.Base_64_Tests is

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_Encode'Access, "Encode");
    Register_Routine (T, Test_Decode'Access, "Decode");
  end Register_Tests;

  -----------------
  -- Test_Encode --
  -----------------
  procedure Test_Encode (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Strings_Equal (Expected => "YW55IGNhcm5hbCBwbGVhc3Vy", Actual => To_String (Encode (To_Byte_Array ("any carnal pleasur"))));
    Assert_Strings_Equal (Expected => "YW55IGNhcm5hbCBwbGVhc3U=", Actual => To_String (Encode (To_Byte_Array ("any carnal pleasu"))));
    Assert_Strings_Equal (Expected => "YW55IGNhcm5hbCBwbGVhcw==", Actual => To_String (Encode (To_Byte_Array ("any carnal pleas"))));
  end;

  -----------------
  -- Test_Decode --
  -----------------
  procedure Test_Decode (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Strings_Equal (Expected => "any carnal pleasur", Actual => To_String (Decode ("YW55IGNhcm5hbCBwbGVhc3Vy")));
    Assert_Strings_Equal (Expected => "any carnal pleasu",  Actual => To_String (Decode ("YW55IGNhcm5hbCBwbGVhc3U=")));
    Assert_Strings_Equal (Expected => "any carnal pleas",   Actual => To_String (Decode ("YW55IGNhcm5hbCBwbGVhcw==")));
  end;

end;
