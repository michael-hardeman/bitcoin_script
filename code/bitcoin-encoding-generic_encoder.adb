
with Interfaces; use Interfaces;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Bitcoin.Encoding.Generic_Encoder is

  ----------------------
  -- Array_Operations --
  ----------------------
  package body Array_Operations is

    -------------------
    -- Count_Leading --
    -------------------
    function Count_Leading (Items : in Array_Type; Value : in Element_Type) return Natural is
      Output : Natural := 0;
    begin
      for I in Items'Range loop
        exit when Items (I) /= Value;
        Output := Natural'Succ (Output);
      end loop;
      return Output;
    end;

    ------------------
    -- Trim_Leading --
    ------------------
    function Trim_Leading (Items : in Array_Type; Value : in Element_Type) return Array_Type is
      Leading_Values : Natural := Count_Leading (Items, Value);
    begin
      if Leading_Values = 0 then return Items; end if;
      return Items (Items'First + Index_Type (Leading_Values) .. Items'Last);
    end;

    --------------------
    -- Append_Leading --
    --------------------
    function Append_Leading (Items : in Array_Type; Value : in Element_Type; Count : in Natural) return Array_Type is
      Values : Array_Type (1 .. Index_Type(Natural'Max (1, Count))) := (others => Value);
    begin
      if Count = 0 then return Items; end if;
      return Values & Items;
    end;
  end;

  --------------------
  -- Instantiations --
  --------------------
  package BAO is new Array_Operations (Byte,              Positive, Byte_Array);
  package CAO is new Array_Operations (Code_Type,         Positive, Code_Type_Array);
  package ESO is new Array_Operations (Encoded_Character, Positive, Encoded_String);

  ------------------------
  -- To_Code_Type_Array --
  ------------------------
  function To_Code_Type_Array (Item : in Byte_Array) return Code_Type_Array is
    Codes         : Code_Type_Array (1 .. Compute_Encoding_Length (Item'Length)) := (others => 0);
    Codes_Length  : Natural     := 0;
    Carry         : Unsigned_16 := 0;
    Codes_Visited : Natural     := 0;
  begin
    for Byte of Item loop
      Carry         := Unsigned_16 (Byte);
      Codes_Visited := 0;
      for I in reverse Codes'Range loop
        exit when (Carry = 0 and Codes_Visited >= Codes_Length);
        Carry := Carry + (256 * Unsigned_16 (Codes (I)));
        Codes (I) := Code_Type (Carry rem BASE);
        Carry := Carry / BASE;
        Codes_Visited := Natural'Succ (Codes_Visited);
      end loop;
      Codes_Length := Codes_Visited;
    end loop;
    return CAO.Trim_Leading (Codes, Code_Type'First);
  end;

  ------------
  -- Encode --
  ------------
  function Encode (Item : in Byte_Array) return Encoded_String is
    Leading_Zeros : Natural         := BAO.Count_Leading (Item, Byte'First);
    Codes         : Code_Type_Array := To_Code_Type_Array (BAO.Trim_Leading  (Item, Byte'First));
    Output        : Encoded_String (Codes'First .. Codes'Last);
  begin
    for I in Codes'Range loop
      Output (I) := Encoded_Character'Val (Codes (I));
    end loop;
    return ESO.Append_Leading (Output, Encoded_Character'First, Leading_Zeros);
  end;

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Item : in Encoded_String) return Byte_Array is
    Bytes         : Byte_Array (1 .. Compute_Decoding_Length (Item'Length)) := (others => 0);
    Bytes_Length  : Natural := 0;
    Bytes_Visited : Natural := 0;
    Decoded       : Code_Type;
    Carry         : Unsigned_16;
  begin
    for Encoded of Item loop
      Decoded       := Encoded_Character'Pos (Encoded);
      Carry         := Unsigned_16 (Decoded);
      Bytes_Visited := 0;
      for I in reverse Bytes'Range loop
        exit when (Carry = 0 and Bytes_Visited >= Bytes_Length);
        Carry := Carry + (BASE * Unsigned_16 (Bytes (I)));
        Bytes (I) := Byte (Carry rem 256);
        Carry := Carry / 256;
        Bytes_Visited := Natural'Succ (Bytes_Visited);
      end loop;
      Bytes_Length := Bytes_Visited;
    end loop;
    return BAO.Trim_Leading (Bytes, Byte'First);
  end;

  ------------
  -- Decode --
  ------------
  function Decode (Item : in Encoded_String) return Byte_Array is
    Leading_Zeros : Natural     := ESO.Count_Leading (Item, Encoded_Character'First);
    Bytes         : Byte_Array  := To_Byte_Array (ESO.Trim_Leading  (Item, Encoded_Character'First));
  begin
    return BAO.Append_Leading (Bytes, Byte'First, Leading_Zeros);
  end;

  ---------------
  -- To_String --
  ---------------
  function To_String (Item : in Encoded_String) return String is
    Output : String (Item'First .. Item'Last) := (others => ' ');
  begin
    for I in Item'Range loop
      Output (I) := To_Character (Item (I));
    end loop;
    return Output;
  end;

  -----------------------
  -- To_Encoded_String --
  -----------------------
  function To_Encoded_String (Item : in String) return Encoded_String is
    Trimmed : String := Trim (Source => Item, Side => Both);
    Output  : Encoded_String (Trimmed'First .. Trimmed'Last) := (others => Encoded_Character'First);
  begin
    for I in Item'Range loop
      Output (I) := To_Encoded_Character (Trimmed (I));
    end loop;
    return Output;
  end;
end;

