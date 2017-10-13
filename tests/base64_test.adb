with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with Bitcoin; use Bitcoin;
with Bitcoin.Base64; use Bitcoin.Base64;
procedure Base64_Test is

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

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Item : in String) return Byte_Array is
    Output : Byte_Array (Item'Range) := (others => 0);
  begin
    for I in Item'Range loop Output (I) := Unsigned_8 (Character'Pos (Item (I))); end loop;
    return Output;
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
  TESTS : constant Test_State_Access_Array := (
    1 => new Test_State'(Encoded_Length => 24, Decoded_Length => 18, Encoded => "YW55IGNhcm5hbCBwbGVhc3Vy", Decoded => To_Byte_Array ("any carnal pleasur")),
    2 => new Test_State'(Encoded_Length => 24, Decoded_Length => 17, Encoded => "YW55IGNhcm5hbCBwbGVhc3U=", Decoded => To_Byte_Array ("any carnal pleasu" )),
    3 => new Test_State'(Encoded_Length => 24, Decoded_Length => 16, Encoded => "YW55IGNhcm5hbCBwbGVhcw==", Decoded => To_Byte_Array ("any carnal pleas"  ))
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
begin
  Run_Tests (TESTS);
end;