
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Crypto.RIPEMD160;      use Bitcoin.Crypto.RIPEMD160;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Crypto.RIPEMD160_Tests is

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_Digest'Access, "Digest");
  end Register_Tests;

  -----------------
  -- Test_Digest --
  -----------------
  procedure Test_Digest (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Byte_Arrays_Equal (
      Expected => (
        16#7c#, 16#03#, 16#82#, 16#0b#, 16#46#,
        16#d6#, 16#b5#, 16#ba#, 16#0e#, 16#f5#,
        16#c5#, 16#d0#, 16#90#, 16#83#, 16#8f#,
        16#6e#, 16#53#, 16#ec#, 16#84#, 16#63#),
      Actual => Digest ("Jackdaws love my big sphinx of quartz."));

    Assert_Byte_Arrays_Equal (
      Expected => (
        16#1e#, 16#d9#, 16#1d#, 16#d9#, 16#08#,
        16#46#, 16#95#, 16#f4#, 16#7f#, 16#68#,
        16#db#, 16#e7#, 16#2b#, 16#a4#, 16#56#,
        16#f2#, 16#be#, 16#36#, 16#b3#, 16#9b#),
      Actual => Digest (
        "A purely peer-to-peer version of electronic cash would allow "  &
        "online payments to be sent directly from one party to another " &
        "without going through a financial institution."));

    Assert_Byte_Arrays_Equal (
      Expected => (
        16#fe#, 16#73#, 16#e6#, 16#26#, 16#7f#,
        16#22#, 16#3a#, 16#51#, 16#01#, 16#44#,
        16#8c#, 16#5c#, 16#b1#, 16#18#, 16#7e#,
        16#0f#, 16#2d#, 16#45#, 16#5b#, 16#aa#),
      Actual => Digest (
        "Ada was originally designed with three overriding concerns: program reliability " &
        "and maintenance, programming as a human activity, and efficiency."));

  end;

end;
