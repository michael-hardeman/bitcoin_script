with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Bitcoin; use Bitcoin;
with Bitcoin.Encoding.Base64; use Bitcoin.Encoding.Base64;

procedure Base64_Test is

  -----------
  -- Image --
  -----------
  function Image (Items : Byte_Array) return String is
    Output : Unbounded_String;
  begin
    Append (Output, "(");
    for I in Items'First .. Items'Last - 1 loop
      Append (Output, Byte'Image (Items (I))); Append (Output, ",");
    end loop;
    Append (Output, Byte'Image (Items (Items'Last))); Append (Output, " )");
    return To_String (Output);
  end;

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Item : in String) return Byte_Array is
    Output : Byte_Array (Item'Range) := (others => 0);
  begin
    for I in Item'Range loop Output (I) := Byte (Character'Pos (Item (I))); end loop;
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
    Put_Line ("Expected : " & Image (Expected_Decoded));
    Put_Line ("Actual   : " & Image (Actual_Decoded));
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
