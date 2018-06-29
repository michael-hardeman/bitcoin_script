
with AUnit.Assertions; use AUnit.Assertions;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Assertions);

with Bitcoin;                       use Bitcoin;
with Bitcoin.Test_Utilities.Common; use Bitcoin.Test_Utilities.Common;

package body Bitcoin.Byte_Array_Tests is

  ---------------
  -- Constants --
  ---------------
  ZERO_SHORT       : constant Byte_Array (1 .. 1)  := ( 1 => 16#00#);
  ZERO_LONG        : constant Byte_Array (1 .. 10) := (others => 16#00#);
  ONE_SHORT        : constant Byte_Array (1 .. 1)  := ( 1 => 16#01#);
  ONE_LONG         : constant Byte_Array (1 .. 10) := (10 => 16#01#, others => 16#00#);
  NATURAL_MAX      : constant Byte_Array (1 .. 4)  := ( 1 => 16#7F#, others => 16#FF#);
  NATURAL_MAX_SUCC : constant Byte_Array (1 .. 4)  := ( 1 => 16#80#, others => 16#00#);
  PANGRAM          : constant Byte_Array           := (16#6a#, 16#61#, 16#63#, 16#6b#, 16#64#,
                                                       16#61#, 16#77#, 16#73#, 16#20#, 16#6c#,
                                                       16#6f#, 16#76#, 16#65#, 16#20#, 16#6d#,
                                                       16#79#, 16#20#, 16#62#, 16#69#, 16#67#,
                                                       16#20#, 16#73#, 16#70#, 16#68#, 16#69#,
                                                       16#6e#, 16#78#, 16#20#, 16#6f#, 16#66#,
                                                       16#20#, 16#71#, 16#75#, 16#61#, 16#72#,
                                                       16#74#, 16#7a#);

  ZERO_SHORT_IMAGE       : constant String := "( 0 )";
  ZERO_LONG_IMAGE        : constant String := "( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )";
  ONE_SHORT_IMAGE        : constant String := "( 1 )";
  ONE_LONG_IMAGE         : constant String := "( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 )";
  NATURAL_MAX_IMAGE      : constant String := "( 127, 255, 255, 255 )";
  NATURAL_MAX_SUCC_IMAGE : constant String := "( 128, 0, 0, 0 )";
  PANGRAM_IMAGE          : constant String := "( 106, 97, 99, 107, 100,"  &
                                               " 97, 119, 115, 32, 108,"  &
                                               " 111, 118, 101, 32, 109," &
                                               " 121, 32, 98, 105, 103,"  &
                                               " 32, 115, 112, 104, 105," &
                                               " 110, 120, 32, 111, 102," &
                                               " 32, 113, 117, 97, 114, 116, 122 )";

  ZERO_SHORT_STRING       : constant String           := "" & Character'Val(0);
  ZERO_LONG_STRING        : constant String (1 .. 10) := (others => Character'Val(0));
  ONE_SHORT_STRING        : constant String           := "" & Character'Val(1);
  ONE_LONG_STRING         : constant String (1 .. 10) := (10 => Character'Val(1), others => Character'Val(0));
  NATURAL_MAX_STRING      : constant String (1 .. 4)  := ( 1 => Character'Val(16#7F#), others => Character'Val(16#FF#));
  NATURAL_MAX_SUCC_STRING : constant String (1 .. 4)  := ( 1 => Character'Val(16#80#), others => Character'Val(0));
  PANGRAM_STRING          : constant String           := "jackdaws love my big sphinx of quartz";

  ZERO_SHORT_NATURAL  : constant Natural := 0;
  ZERO_LONG_NATURAL   : constant Natural := 0;
  ONE_SHORT_NATURAL   : constant Natural := 1;
  ONE_LONG_NATURAL    : constant Natural := 1;
  NATURAL_MAX_NATURAL : constant Natural := Natural'Last;

  ----------
  -- Name --
  ----------
  function Name (T : TC) return Message_String is
    pragma Unreferenced (T);
  begin
    return AUnit.Format ("Testing Bitcoin.Byte_Array operations");
  end Name;

  --------------------
  -- Register_Tests --
  --------------------
  procedure Register_Tests (T : in out TC) is
    use AUnit.Test_Cases.Registration;
  begin
    Register_Routine (T, Test_Image'Access,               "Ensure Bitcoin.Image returns a string visualizing the byte array");
    Register_Routine (T, Test_To_String'Access,           "Ensure Bitcoin.To_String converts a byte array directly into a String by getting Character'Val");
    Register_Routine (T, Test_To_Byte_Array'Access,       "Ensure Bitcoin.To_Byte_Array converts a string into a byte array of Character'Pos codes");
    Register_Routine (T, Test_Count_Leading_Zeros'Access, "Ensure Bitcoin.Count_Leading_Zeros returns the correct number of leading zeros in a byte array");
    Register_Routine (T, Test_Trim_Leading_Zeros'Access,  "Ensure Bitcoin.Trim_Leading_Zeros removes the leading zeros from a byte array");
    Register_Routine (T, Test_Is_Zero'Access,             "Ensure Bitcoin.Is_Zero returns true when all bytes = 0 and false in all other cases");
    Register_Routine (T, Test_Is_One'Access,              "Ensure Bitcoin.Is_One returns true when the last byte = 1 and all others bytes = 0, and false in all other cases");
    Register_Routine (T, Test_To_Natural'Access,          "Ensure Bitcoin.To_Natural converts a byte array into a valid Natural, and raises an exception in all other cases");
  end Register_Tests;

  ----------------
  -- Test_Image --
  ----------------
  procedure Test_Image (Test : in out Test_Cases.Test_Case'Class) is
  begin
    Assert_Strings_Equal (ZERO_SHORT_IMAGE,       Image (ZERO_SHORT));
    Assert_Strings_Equal (ZERO_LONG_IMAGE,        Image (ZERO_LONG));
    Assert_Strings_Equal (ONE_SHORT_IMAGE,        Image (ONE_SHORT));
    Assert_Strings_Equal (ONE_LONG_IMAGE,         Image (ONE_LONG));
    Assert_Strings_Equal (NATURAL_MAX_IMAGE,      Image (NATURAL_MAX));
    Assert_Strings_Equal (NATURAL_MAX_SUCC_IMAGE, Image (NATURAL_MAX_SUCC));
    Assert_Strings_Equal (PANGRAM_IMAGE,          Image (PANGRAM));
  end;

  --------------------
  -- Test_To_String --
  --------------------
  procedure Test_To_String (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Strings_Equal (ZERO_SHORT_STRING,       To_String (ZERO_SHORT));
    Assert_Strings_Equal (ZERO_LONG_STRING,        To_String (ZERO_LONG));
    Assert_Strings_Equal (ONE_SHORT_STRING,        To_String (ONE_SHORT));
    Assert_Strings_Equal (ONE_LONG_STRING,         To_String (ONE_LONG));
    Assert_Strings_Equal (NATURAL_MAX_STRING,      To_String (NATURAL_MAX));
    Assert_Strings_Equal (NATURAL_MAX_SUCC_STRING, To_String (NATURAL_MAX_SUCC));
    Assert_Strings_Equal (PANGRAM_STRING,          To_String (PANGRAM));
  end;

  ------------------------
  -- Test_To_Byte_Array --
  ------------------------
  procedure Test_To_Byte_Array (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Byte_Arrays_Equal (ZERO_SHORT,       To_Byte_Array (ZERO_SHORT_STRING));
    Assert_Byte_Arrays_Equal (ZERO_LONG,        To_Byte_Array (ZERO_LONG_STRING));
    Assert_Byte_Arrays_Equal (ONE_SHORT,        To_Byte_Array (ONE_SHORT_STRING));
    Assert_Byte_Arrays_Equal (ONE_LONG,         To_Byte_Array (ONE_LONG_STRING));
    Assert_Byte_Arrays_Equal (NATURAL_MAX,      To_Byte_Array (NATURAL_MAX_STRING));
    Assert_Byte_Arrays_Equal (NATURAL_MAX_SUCC, To_Byte_Array (NATURAL_MAX_SUCC_STRING));
    Assert_Byte_Arrays_Equal (PANGRAM,          To_Byte_Array (PANGRAM_STRING));
  end;

  ------------------------------
  -- Test_Count_Leading_Zeros --
  ------------------------------
  procedure Test_Count_Leading_Zeros (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Naturals_Equal (1,  Count_Leading_Zeros (ZERO_SHORT));
    Assert_Naturals_Equal (10, Count_Leading_Zeros (ZERO_LONG));
    Assert_Naturals_Equal (0,  Count_Leading_Zeros (ONE_SHORT));
    Assert_Naturals_Equal (9,  Count_Leading_Zeros (ONE_LONG));
    Assert_Naturals_Equal (0,  Count_Leading_Zeros (PANGRAM));
  end;

  -----------------------------
  -- Test_Trim_Leading_Zeros --
  -----------------------------
  procedure Test_Trim_Leading_Zeros (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Naturals_Equal    (0,         Trim_Leading_Zeros (ZERO_SHORT)'Length);
    Assert_Naturals_Equal    (0,         Trim_Leading_Zeros (ZERO_LONG)'Length);
    Assert_Byte_Arrays_Equal (ONE_SHORT, Trim_Leading_Zeros (ONE_SHORT));
    Assert_Byte_Arrays_Equal (ONE_SHORT, Trim_Leading_Zeros (ONE_LONG));
    Assert_Byte_Arrays_Equal (PANGRAM,   Trim_Leading_Zeros (PANGRAM));
  end;

  ------------------
  -- Test_Is_Zero --
  ------------------
  procedure Test_Is_Zero (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Booleans_Equal (TRUE,  Is_Zero (ZERO_SHORT));
    Assert_Booleans_Equal (TRUE,  Is_Zero (ZERO_LONG));
    Assert_Booleans_Equal (FALSE, Is_Zero (ONE_SHORT));
    Assert_Booleans_Equal (FALSE, Is_Zero (ONE_LONG));
    Assert_Booleans_Equal (FALSE, Is_Zero (PANGRAM));
  end;

  -----------------
  -- Test_Is_One --
  -----------------
  procedure Test_Is_One (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Booleans_Equal (FALSE, Is_One (ZERO_SHORT));
    Assert_Booleans_Equal (FALSE, Is_One (ZERO_LONG));
    Assert_Booleans_Equal (TRUE,  Is_One (ONE_SHORT));
    Assert_Booleans_Equal (TRUE,  Is_One (ONE_LONG));
    Assert_Booleans_Equal (FALSE, Is_One (PANGRAM));
  end;

  procedure To_Natural_Natural_Max_Successor is Ignore : Natural; begin Ignore := To_Natural (NATURAL_MAX_SUCC); end;
  procedure To_Natural_Pangram               is Ignore : Natural; begin Ignore := To_Natural (PANGRAM);         end;

  ----------------
  -- Test_Image --
  ----------------
  procedure Test_To_Natural (Test : in out Test_Cases.Test_Case'Class) is begin
    Assert_Naturals_Equal (ZERO_SHORT_NATURAL,  To_Natural (ZERO_SHORT));
    Assert_Naturals_Equal (ZERO_LONG_NATURAL,   To_Natural (ZERO_LONG));
    Assert_Naturals_Equal (ONE_SHORT_NATURAL,   To_Natural (ONE_SHORT));
    Assert_Naturals_Equal (ONE_LONG_NATURAL,    To_Natural (ONE_LONG));
    Assert_Naturals_Equal (NATURAL_MAX_NATURAL, To_Natural (NATURAL_MAX));

    Assert_Exception      (To_Natural_Natural_Max_Successor'Access, "Expected Natural'Last + 1 to throw an error.");
    Assert_Exception      (To_Natural_Pangram'Access,               "Expected the Jackdaws pangram to throw an error.");
  end;

end;
