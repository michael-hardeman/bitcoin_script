
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Encoding.Base58;       use Bitcoin.Encoding.Base58;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Encoding.Base_58_Tests is

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
    -- 0 Leading 0s and 0 Trailing 0s
    Assert_Strings_Equal (Expected => "6UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS", Actual => To_String (Encode ((
       1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,
       6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
      11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
      16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#,
      21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#))));

    -- 1 Leading 0s and 0 Trailing 0s
    Assert_Strings_Equal (Expected => "16UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS", Actual => To_String (Encode ((
       1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
       6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
      11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
      16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
      21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#))));

    -- 2 Leading 0s and 0 Trailing 0s
    Assert_Strings_Equal (Expected => "116UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS", Actual => To_String (Encode ((
       1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,
       6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#,
      11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#,
      16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#,
      21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#,
      26 => 16#87#))));

    -- 0 Leading 0s and 1 Trailing 0s
    Assert_Strings_Equal (Expected => "RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M", Actual => To_String (Encode ((
       1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,
       6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
      11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
      16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#,
      21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#, 25 => 16#00#))));

    -- 0 Leading 0s and 2 Trailing 0s
    Assert_Strings_Equal (Expected => "2qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H", Actual => To_String (Encode ((
       1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,
       6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
      11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
      16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#,
      21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#, 25 => 16#00#,
      26 => 16#00#))));

    -- 1 Leading 0s and 1 Trailing 0s
    Assert_Strings_Equal (Expected => "1RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M", Actual => To_String (Encode ((
       1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
       6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
      11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
      16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
      21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#,
      26 => 16#00#))));

    -- 1 Leading 0s and 2 Trailing 0s
    Assert_Strings_Equal (Expected => "12qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H", Actual => To_String (Encode ((
       1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
       6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
      11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
      16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
      21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#,
      26 => 16#00#, 27 => 16#00#))));

    -- 2 Leading 0s and 1 Trailing 0s
    Assert_Strings_Equal (Expected => "11RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M", Actual => To_String (Encode ((
       1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,
       6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#,
      11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#,
      16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#,
      21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#,
      26 => 16#87#, 27 => 16#00#))));

    -- 2 Leading 0s and 2 Trailing 0s
    Assert_Strings_Equal (Expected => "112qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H", Actual => To_String (Encode ((
       1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,
       6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#,
      11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#,
      16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#,
      21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#,
      26 => 16#87#, 27 => 16#00#, 28 => 16#00#))));
  end;

  -----------------
  -- Test_Decode --
  -----------------
  procedure Test_Decode (Test : in out Test_Cases.Test_Case'Class) is
  begin

    -- 0 Leading 0s and 0 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,
         6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
        11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
        16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#,
        21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#),
      Actual => Decode ("6UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS"));

    -- 1 Leading 0s and 0 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
         6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
        11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
        16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
        21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#),
      Actual => Decode ("16UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS"));

    -- 2 Leading 0s and 0 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,
         6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#,
        11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#,
        16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#,
        21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#,
        26 => 16#87#),
      Actual => Decode ("116UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS"));

    -- 0 Leading 0s and 1 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,
         6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
        11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
        16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#,
        21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#, 25 => 16#00#),
      Actual => Decode ("RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M"));

    -- 0 Leading 0s and 2 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,
         6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
        11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
        16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#,
        21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#, 25 => 16#00#,
        26 => 16#00#),
      Actual => Decode ("2qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H"));

    -- 1 Leading 0s and 1 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
         6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
        11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
        16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
        21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#,
        26 => 16#00#),
      Actual => Decode ("1RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M"));

    -- 1 Leading 0s and 2 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
         6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
        11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
        16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
        21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#,
        26 => 16#00#, 27 => 16#00#),
      Actual => Decode ("12qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H"));

    -- 2 Leading 0s and 1 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,
         6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#,
        11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#,
        16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#,
        21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#,
        26 => 16#87#, 27 => 16#00#),
      Actual => Decode ("11RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M"));

    -- 2 Leading 0s and 2 Trailing 0s
    Assert_Byte_Arrays_Equal (
      Expected => (
         1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,
         6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#,
        11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#,
        16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#,
        21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#,
        26 => 16#87#, 27 => 16#00#, 28 => 16#00#),
      Actual => Decode ("112qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H"));
  end;

end;
