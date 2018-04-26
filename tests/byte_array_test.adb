with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Bitcoin; use Bitcoin;

procedure Byte_Array_Test is

  ---------------
  -- Constants --
  ---------------
  ZERO_SHORT         : constant Byte_Array (1 .. 1)  := ( 1 => 16#00#);
  ZERO_LONG          : constant Byte_Array (1 .. 10) := (others => 16#00#);
  ONE_SHORT          : constant Byte_Array (1 .. 1)  := ( 1 => 16#01#);
  ONE_LONG           : constant Byte_Array (1 .. 10) := (10 => 16#01#, others => 16#00#);
  NATURAL_MAX        : constant Byte_Array (1 .. 4)  := ( 1 => 16#7F#, others => 16#FF#);
  NATURAL_MAX_SUCC   : constant Byte_Array (1 .. 4)  := ( 1 => 16#80#, others => 16#00#);
  EXAMPLE_ONE        : constant Byte_Array           := (16#6a#, 16#61#, 16#63#, 16#6b#, 16#64#,
                                                         16#61#, 16#77#, 16#73#, 16#20#, 16#6c#,
                                                         16#6f#, 16#76#, 16#65#, 16#20#, 16#6d#,
                                                         16#79#, 16#20#, 16#62#, 16#69#, 16#67#,
                                                         16#20#, 16#73#, 16#70#, 16#68#, 16#69#,
                                                         16#6e#, 16#78#, 16#20#, 16#6f#, 16#66#,
                                                         16#20#, 16#71#, 16#75#, 16#61#, 16#72#,
                                                         16#74#, 16#7a#);
  ZERO_SHORT_IMAGE        : constant String           := "( 0 )";
  ZERO_LONG_IMAGE         : constant String           := "( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )";
  ONE_SHORT_IMAGE         : constant String           := "( 1 )";
  ONE_LONG_IMAGE          : constant String           := "( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 )";
  NATURAL_MAX_IMAGE       : constant String           := "( 127, 255, 255, 255 )";
  NATURAL_MAX_SUCC_IMAGE  : constant String           := "( 128, 0, 0, 0 )";
  EXAMPLE_ONE_IMAGE       : constant String           := "( 106, 97, 99, 107, 100, 97, 119, 115, 32, 108, 111, 118, 101, 32, 109, 121, 32, 98, 105, 103, 32, 115, 112, 104, 105, 110, 120, 32, 111, 102, 32, 113, 117, 97, 114, 116, 122 )";
  ZERO_SHORT_STRING       : constant String           := "" & Character'Val(0);
  ZERO_LONG_STRING        : constant String (1 .. 10) := (others => Character'Val(0));
  ONE_SHORT_STRING        : constant String           := "" & Character'Val(1);
  ONE_LONG_STRING         : constant String (1 .. 10) := (10 => Character'Val(1), others => Character'Val(0));
  NATURAL_MAX_STRING      : constant String (1 .. 4)  := ( 1 => Character'Val(16#7F#), others => Character'Val(16#FF#));
  NATURAL_MAX_SUCC_STRING : constant String (1 .. 4)  := ( 1 => Character'Val(16#80#), others => Character'Val(0));
  EXAMPLE_ONE_STRING      : constant String           := "jackdaws love my big sphinx of quartz";
  ZERO_SHORT_NATURAL      : constant Natural          := 0;
  ZERO_LONG_NATURAL       : constant Natural          := 0;
  ONE_SHORT_NATURAL       : constant Natural          := 1;
  ONE_LONG_NATURAL        : constant Natural          := 1;
  NATURAL_MAX_NATURAL     : constant Natural          := Natural'Last;

  -------------------
  -- Discrete_Test --
  -------------------
  generic
    type Result_Type is private;
    with function "=" (Left, Right : in Result_Type) return Boolean;
    with function Image (Item : in Result_Type) return String;
  function Discrete_Test (Expected : in Result_Type; Actual : in Result_Type) return Boolean;
  function Discrete_Test (Expected : in Result_Type; Actual : in Result_Type) return Boolean is
    Result : Boolean := Expected = Actual;
  begin
    if not Result then
      Put_Line ("FAILURE");
      Put_Line ("  Expected: " & Image (Expected));
      Put_Line ("  Actual  : " & Image (Actual));
    end if;
    return Result;
  exception
    when Error: others =>
      Put_Line ("FAILURE");
      Put_Line ("  An unhandled exception has occoured during test: " & Image (Expected));
      Put_Line ("  " & Exception_Name        (Error));
      Put_Line ("  " & Exception_Message     (Error));
      Put_Line ("  " & Exception_Information (Error));
      return FALSE;
  end;

  --------------------
  -- Aggregate_Test --
  --------------------
  generic
    type Result_Type (<>) is private;
    with function "=" (Left, Right : in Result_Type) return Boolean;
    with function Image (Item : in Result_Type) return String;
  function Aggregate_Test (Expected : in Result_Type; Actual : in Result_Type) return Boolean;
  function Aggregate_Test (Expected : in Result_Type; Actual : in Result_Type) return Boolean is
    Result : Boolean := Expected = Actual;
  begin
    if not Result then
      Put_Line ("FAILURE");
      Put_Line ("  Expected: " & Image (Expected));
      Put_Line ("  Actual  : " & Image (Actual));
    end if;
    return Result;
  exception
    when Error: others =>
      Put_Line ("FAILURE");
      Put_Line ("  An unhandled exception has occured during test: " & Image (Expected));
      Put_Line ("  " & Exception_Name        (Error));
      Put_Line ("  " & Exception_Message     (Error));
      Put_Line ("  " & Exception_Information (Error));
      return FALSE;
  end;

  -----------------------------
  -- Discrete_Exception_Test --
  -----------------------------
  generic
    type Input_Type is private;
    with procedure Should_Raise (Item : in Input_Type);
    with function Image (Item : in Input_Type) return String;
  function Discrete_Exception_Test (Name : in String; Input : in Input_Type) return Boolean;
  function Discrete_Exception_Test (Name : in String; Input : in Input_Type) return Boolean is
  begin
    Should_Raise (Input);
    Put_Line ("FAILURE");
    Put_Line ("  Expected exception did not occur during test: " & Image (Input));
    Put_Line ("  Expected name: " & Name);
    return FALSE;
  exception
    when Error: others =>
      If Exception_Name (Error) = Name then return TRUE; end if;
      Put_Line ("FAILURE");
      Put_Line ("  Expected exception did not occur during test: " & Image (Input));
      Put_Line ("  Expected name   : " & Name    & " to equal " & Exception_Name    (Error));
      Put_Line ("  Expected message: " & Exception_Message     (Error));
      Put_Line ("  Exception info  : " & Exception_Information (Error));
      return FALSE;
  end;

  ------------------------------
  -- Aggregate_Exception_Test --
  ------------------------------
  generic
    type Input_Type (<>) is private;
    with procedure Should_Raise (Item : in Input_Type);
    with function Image (Item : in Input_Type) return String;
  function Aggregate_Exception_Test (Name : in String; Input : in Input_Type) return Boolean;
  function Aggregate_Exception_Test (Name : in String; Input : in Input_Type) return Boolean is
  begin
    Should_Raise (Input);
    Put_Line ("FAILURE");
    Put_Line ("  Expected exception did not occur during test: " & Image (Input));
    Put_Line ("  Expected name: " & Name);
    return FALSE;
  exception
    when Error: others =>
      If Exception_Name (Error) = Name then return TRUE; end if;
      Put_Line ("FAILURE");
      Put_Line ("  Expected exception did not occur during test: " & Image (Input));
      Put_Line ("  Expected name   : " & Name & " to equal " & Exception_Name (Error));
      Put_Line ("  Expected message: " & Exception_Message     (Error));
      Put_Line ("  Exception info  : " & Exception_Information (Error));
      return FALSE;
  end;

  function  String_Image   (Item : in String) return String is (Item);
  procedure Run_To_Natural (Item : in Byte_Array) is Ignore : Natural := To_Natural (Item); begin null; end;

  function String_Test is new Aggregate_Test (String, "=", String_Image);
  function Byte_Array_Test is new Aggregate_Test (Byte_Array, "=", Image);
  function Natural_Test is new Discrete_Test (Natural, "=", Natural'Image);
  function Natural_Exception_Test is new Aggregate_Exception_Test (Byte_Array, Run_To_Natural, Image);

  procedure Image_Tests is
    Total_Result : Boolean := TRUE;
  begin
    Put_Line ("-----------");
    Put_Line ("-- Image --");
    Put_Line ("-----------");
    Total_Result := Total_Result and String_Test (ZERO_SHORT_IMAGE,       Image (ZERO_SHORT));
    Total_Result := Total_Result and String_Test (ZERO_LONG_IMAGE,        Image (ZERO_LONG));
    Total_Result := Total_Result and String_Test (ONE_SHORT_IMAGE,        Image (ONE_SHORT));
    Total_Result := Total_Result and String_Test (ONE_LONG_IMAGE,         Image (ONE_LONG));
    Total_Result := Total_Result and String_Test (NATURAL_MAX_IMAGE,      Image (NATURAL_MAX));
    Total_Result := Total_Result and String_Test (NATURAL_MAX_SUCC_IMAGE, Image (NATURAL_MAX_SUCC));
    Total_Result := Total_Result and String_Test (EXAMPLE_ONE_IMAGE,      Image (EXAMPLE_ONE));
    Put_Line ("ALL PASSING? " & Boolean'Image (Total_Result));
    New_Line;
  end;

  procedure To_String_Tests is
    Total_Result : Boolean := TRUE;
  begin
    Put_Line ("---------------");
    Put_Line ("-- To_String --");
    Put_Line ("---------------");
    Total_Result := Total_Result and String_Test (ZERO_SHORT_STRING,       To_String (ZERO_SHORT));
    Total_Result := Total_Result and String_Test (ZERO_LONG_STRING,        To_String (ZERO_LONG));
    Total_Result := Total_Result and String_Test (ONE_SHORT_STRING,        To_String (ONE_SHORT));
    Total_Result := Total_Result and String_Test (ONE_LONG_STRING,         To_String (ONE_LONG));
    Total_Result := Total_Result and String_Test (NATURAL_MAX_STRING,      To_String (NATURAL_MAX));
    Total_Result := Total_Result and String_Test (NATURAL_MAX_SUCC_STRING, To_String (NATURAL_MAX_SUCC));
    Total_Result := Total_Result and String_Test (EXAMPLE_ONE_STRING,      To_String (EXAMPLE_ONE));
    Put_Line ("ALL PASSING? " & Boolean'Image (Total_Result));
    New_Line;
  end;

  procedure To_Byte_Array_Tests is
    Total_Result : Boolean := TRUE;
  begin
    Put_Line ("-------------------");
    Put_Line ("-- To_Byte_Array --");
    Put_Line ("-------------------");
    Total_Result := Total_Result and Byte_Array_Test (ZERO_SHORT,       To_Byte_Array (ZERO_SHORT_STRING));
    Total_Result := Total_Result and Byte_Array_Test (ZERO_LONG,        To_Byte_Array (ZERO_LONG_STRING));
    Total_Result := Total_Result and Byte_Array_Test (ONE_SHORT,        To_Byte_Array (ONE_SHORT_STRING));
    Total_Result := Total_Result and Byte_Array_Test (ONE_LONG,         To_Byte_Array (ONE_LONG_STRING));
    Total_Result := Total_Result and Byte_Array_Test (NATURAL_MAX,      To_Byte_Array (NATURAL_MAX_STRING));
    Total_Result := Total_Result and Byte_Array_Test (NATURAL_MAX_SUCC, To_Byte_Array (NATURAL_MAX_SUCC_STRING));
    Total_Result := Total_Result and Byte_Array_Test (EXAMPLE_ONE,      To_Byte_Array (EXAMPLE_ONE_STRING));
    Put_Line ("ALL PASSING? " & Boolean'Image (Total_Result));
    New_Line;
  end;

  procedure To_Natural_Tests is
    Total_Result : Boolean := TRUE;
  begin
    Put_Line ("----------------");
    Put_Line ("-- To_Natural --");
    Put_Line ("----------------");
    Total_Result := Total_Result and Natural_Test (ZERO_SHORT_NATURAL,  To_Natural (ZERO_SHORT));
    Total_Result := Total_Result and Natural_Test (ZERO_LONG_NATURAL,   To_Natural (ZERO_LONG));
    Total_Result := Total_Result and Natural_Test (ONE_SHORT_NATURAL,   To_Natural (ONE_SHORT));
    Total_Result := Total_Result and Natural_Test (ONE_LONG_NATURAL,    To_Natural (ONE_LONG));
    Total_Result := Total_Result and Natural_Test (NATURAL_MAX_NATURAL, To_Natural (NATURAL_MAX));

    Total_Result := Total_Result and Natural_Exception_Test ( "CONSTRAINT_ERROR", NATURAL_MAX_SUCC);
    Total_Result := Total_Result and Natural_Exception_Test ( "CONSTRAINT_ERROR", EXAMPLE_ONE);
    Put_Line ("ALL PASSING? " & Boolean'Image (Total_Result));
    New_Line;
  end;

begin
  Image_Tests;
  To_String_Tests;
  To_Byte_Array_Tests;
  To_Natural_Tests;
end;
