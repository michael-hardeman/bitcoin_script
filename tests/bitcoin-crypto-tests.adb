
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

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_Sign'Access, "Sign");
  end Register_Tests;

  ---------------
  -- Test_Sign --
  ---------------
  procedure Test_Sign (Test : in out Test_Cases.Test_Case'Class) is
  begin
    null;
  end;

end;
