with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with Bitcoin; use Bitcoin;
with Bitcoin.Encoding.Base58; use Bitcoin.Encoding.Base58;

procedure Base58_Test is

  ---------------
  -- Serialize --
  ---------------
  function Serialize (Items : Byte_Array) return String is
    Output : Unbounded_String;
  begin
    Append (Output, "(");
    for I in Items'First .. Items'Last - 1 loop
      Append (Output, Unsigned_8'Image (Items (I))); Append (Output, ",");
    end loop;
    Append (Output, Unsigned_8'Image (Items (Items'Last))); Append (Output, " )");
    return To_String (Output);
  end;

  -----------
  -- Types --
  -----------
  type Test_State (Encoded_Length : Positive; Decoded_Length : Positive) is record
    Encoded : Encoded_String (1 .. Encoded_Length);
    Decoded : Byte_Array (1 .. Decoded_Length);
  end record;
  type Test_State_Access is access all Test_State;
  type Test_State_Access_Array is array (Positive range <>) of Test_State_Access;

  -----------
  -- TESTS --
  -----------
  -- All test cases verified at: https://www.mobilefish.com/services/cryptocurrency/brainwallet.html#converter
  TESTS : constant Test_State_Access_Array := (
    -- no Leading 0s and no Trailing 0s
    1 => new Test_State'(Encoded_Length => 33, Decoded_Length => 24, Encoded => "6UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS", Decoded => (
       1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,  
       6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
      11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
      16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#, 
      21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#)),
    -- 1 Leading 0 and no Trailing 0s
    2 => new Test_State'(Encoded_Length => 34, Decoded_Length => 25, Encoded => "16UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS", Decoded => (
       1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
       6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
      11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
      16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
      21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#)),
    -- 2 Leading 0s and no Trailing 0s
    3 => new Test_State'(Encoded_Length => 35, Decoded_Length => 26, Encoded => "116UjcYNBG9GTK4uq2f7yYEbuifqCzoLMGS", Decoded => (
        1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,  
        6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#, 
       11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#, 
       16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#, 
       21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#, 
       26 => 16#87#)),
    -- no Leading 0s and 1 Trailing 0
    4 => new Test_State'(Encoded_Length => 34, Decoded_Length => 25, Encoded => "RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M", Decoded => (
       1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,  
       6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
      11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
      16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#, 
      21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#, 25 => 16#00#)),
    -- no Leading 0s and 2 Trailing 0s
    5 => new Test_State'(Encoded_Length => 36, Decoded_Length => 26, Encoded => "2qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H", Decoded => (
       1 => 16#3C#,  2 => 16#17#,  3 => 16#6E#,  4 => 16#65#,  5 => 16#9B#,  
       6 => 16#EA#,  7 => 16#0F#,  8 => 16#29#,  9 => 16#A3#, 10 => 16#E9#,
      11 => 16#BF#, 12 => 16#78#, 13 => 16#80#, 14 => 16#C1#, 15 => 16#12#,
      16 => 16#B1#, 17 => 16#B3#, 18 => 16#1B#, 19 => 16#4D#, 20 => 16#C8#, 
      21 => 16#26#, 22 => 16#26#, 23 => 16#81#, 24 => 16#87#, 25 => 16#00#,
      26 => 16#00#)),
    -- 1 Leading 0 and 1 Trailing 0
    6 => new Test_State'(Encoded_Length => 35, Decoded_Length => 26, Encoded => "1RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M", Decoded => (
       1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
       6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
      11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
      16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
      21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#,
      26 => 16#00#)),
    -- 1 Leading 0 and 2 Trailing 0s
    7 => new Test_State'(Encoded_Length => 37, Decoded_Length => 27, Encoded => "12qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H", Decoded => (
       1 => 16#00#,  2 => 16#3C#,  3 => 16#17#,  4 => 16#6E#,  5 => 16#65#,
       6 => 16#9B#,  7 => 16#EA#,  8 => 16#0F#,  9 => 16#29#, 10 => 16#A3#,
      11 => 16#E9#, 12 => 16#BF#, 13 => 16#78#, 14 => 16#80#, 15 => 16#C1#,
      16 => 16#12#, 17 => 16#B1#, 18 => 16#B3#, 19 => 16#1B#, 20 => 16#4D#,
      21 => 16#C8#, 22 => 16#26#, 23 => 16#26#, 24 => 16#81#, 25 => 16#87#,
      26 => 16#00#, 27 => 16#00#)),
    -- 2 Leading 0s and 1 Trailing 0
    8 => new Test_State'(Encoded_Length => 36, Decoded_Length => 27, Encoded => "11RBR5sSUHqVD8kGAzKFnYs35gvPmx7WQT7M", Decoded => (
        1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,  
        6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#, 
       11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#, 
       16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#, 
       21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#, 
       26 => 16#87#, 27 => 16#00#)),
    -- 2 Leading 0s and 2 Trailing 0s
    9 => new Test_State'(Encoded_Length => 38, Decoded_Length => 28, Encoded => "112qiyJVdRTK1WZBwy5yaG3drBhCfXQFinWF1H", Decoded => (
        1 => 16#00#,  2 => 16#00#,  3 => 16#3C#,  4 => 16#17#,  5 => 16#6E#,  
        6 => 16#65#,  7 => 16#9B#,  8 => 16#EA#,  9 => 16#0F#, 10 => 16#29#, 
       11 => 16#A3#, 12 => 16#E9#, 13 => 16#BF#, 14 => 16#78#, 15 => 16#80#, 
       16 => 16#C1#, 17 => 16#12#, 18 => 16#B1#, 19 => 16#B3#, 20 => 16#1B#, 
       21 => 16#4D#, 22 => 16#C8#, 23 => 16#26#, 24 => 16#26#, 25 => 16#81#, 
       26 => 16#87#, 27 => 16#00#, 28 => 16#00#))
  );

  --------------
  -- Run_Test --
  --------------
  procedure Run_Test (Test : in Test_State) is
    Expected_Encoded : Encoded_String := Test.Encoded;
    Actual_Encoded   : Encoded_String := Encode (Test.Decoded);
    Expected_Decoded : Byte_Array     := Test.Decoded;
    Actual_Decoded   : Byte_Array     := Decode (Test.Encoded);
  begin
    Put_Line ("--------------"); 
    Put_Line ("-- ENCODING --");
    Put_Line ("--------------");
    Put_Line ("Expected : " & To_String (Expected_Encoded));
    Put_Line ("Actual   : " & To_String (Actual_Encoded));
    Put_Line ("--------------");
    Put_Line ("-- DECODING --");
    Put_Line ("--------------");
    Put_Line ("Expected : " & Serialize (Expected_Decoded));
    Put_Line ("Actual   : " & Serialize (Actual_Decoded));
  end;

  ---------------
  -- Run_Tests --
  ---------------
  procedure Run_Tests (Tests : in Test_State_Access_Array) is
  begin
    for Test of Tests loop
      Run_Test (Test.all);
    end loop;
  end;
  
  procedure Free_Test is new Ada.Unchecked_Deallocation (Test_State, Test_State_Access);
  
  ----------------
  -- Free_Tests --
  ----------------
  procedure Free_Tests (Tests : in Test_State_Access_Array) is
  begin
    for I in Tests'Range loop
      declare Test : Test_State_Access := Tests (I); begin
        Free_Test (Test);
      end;
    end loop;
  end;
begin
  Run_Tests (TESTS);
  Free_Tests (TESTS);
end;
