package body Bitcoin.Encoding.Base58 is

  subtype Code_Type is Unsigned_8 range 0 .. 57;
  type Code_Type_Array is array (Positive range <>) of Code_Type;

  ------------------
  -- To_Character --
  ------------------
  function To_Character (Item : Encoded_Character) return Character is (
    case Item is
      when '1'..'9' => Character'Val ( 49 + (Encoded_Character'Pos (Item)     )),
      when 'A'..'H' => Character'Val ( 65 + (Encoded_Character'Pos (Item) -  9)),
      when 'J'..'P' => Character'Val ( 74 + (Encoded_Character'Pos (Item) - 17)),
      when 'Q'..'Z' => Character'Val ( 81 + (Encoded_Character'Pos (Item) - 23)),
      when 'a'..'k' => Character'Val ( 97 + (Encoded_Character'Pos (Item) - 33)),
      when 'm'..'z' => Character'Val (109 + (Encoded_Character'Pos (Item) - 44)));

  --------------------------
  -- To_Encoded_Character --
  --------------------------
  function To_Encoded_Character (Item : Character) return Encoded_Character is
  begin
    case Item is
      when '1'..'9' => return Encoded_Character'Val (     (Character'Pos (Item) -  49));
      when 'A'..'H' => return Encoded_Character'Val ( 9 + (Character'Pos (Item) -  65));
      when 'J'..'P' => return Encoded_Character'Val (17 + (Character'Pos (Item) -  74));
      when 'Q'..'Z' => return Encoded_Character'Val (23 + (Character'Pos (Item) -  81));
      when 'a'..'k' => return Encoded_Character'Val (33 + (Character'Pos (Item) -  97));
      when 'm'..'z' => return Encoded_Character'Val (44 + (Character'Pos (Item) - 109));
      when others   => raise Malformed_Base58;
    end case;
  end;

  ---------------
  -- To_String --
  ---------------
  function To_String (Item : Encoded_String) return String is
    Result : String (Item'Range);
  begin
    for I in Item'Range loop Result (I) := To_Character (Item (I)); end loop;
    return Result;
  end;

  -----------------------
  -- To_Encoded_String --
  -----------------------
  function To_Encoded_String (Item : in String) return Encoded_String is
    Trimmed : String := Trim (Item, Both);
    Result  : Encoded_String (Trimmed'Range);
  begin
    for I in Trimmed'Range loop Result (I) := To_Encoded_Character(Trimmed (I)); end loop;
    return Result;
  end;

  -------------------------
  -- Count_Leading_Zeros --
  -------------------------
  function Count_Leading_Zeros (Items : in Byte_Array) return Natural is
    Output : Natural := 0;
  begin
    for I in Items'Range loop
      exit when Items (I) /= 0;
      Output := Natural'Succ (Output);
    end loop;
    return Output;
  end;
  -------------------------
  -- Count_Leading_Zeros --
  -------------------------
  function Count_Leading_Zeros (Items : in Code_Type_Array) return Natural is
    Output : Natural := 0;
  begin
    for I in Items'Range loop
      exit when Items (I) /= 0;
      Output := Natural'Succ (Output);
    end loop;
    return Output;
  end;

  ------------------------
  -- Trim_Leading_Zeros --
  ------------------------
  function Trim_Leading_Zeros (Items : in Byte_Array) return Byte_Array is (Items (Items'First + Count_Leading_Zeros (Items) .. Items'Last));
  function Trim_Leading_Zeros (Items : in Code_Type_Array) return Code_Type_Array is (Items (Items'First + Count_Leading_Zeros (Items) .. Items'Last));

  ------------------------
  -- To_Code_Type_Array --
  ------------------------
  function To_Code_Type_Array (Item : in Byte_Array) return Code_Type_Array is
    Codes        : Code_Type_Array (1 .. Item'Length * 138 / 100 + 1) := (others => 0);
    Codes_Length : Natural := 0;
  begin
    for Byte of Item loop
      declare
        Carry         : Unsigned_16 := Unsigned_16 (Byte);
        Codes_Visited : Natural     := 0;
      begin
        for I in reverse Codes'Range loop
          exit when (Carry = 0 and Codes_Visited >= Codes_Length);
          Carry := Carry + (256 * Unsigned_16 (Codes (I)));
          Codes (I) := Code_Type (Carry rem 58);
          Carry := Carry / 58;
          Codes_Visited := Natural'Succ (Codes_Visited);
        end loop;
        Codes_Length := Codes_Visited;
      end;
    end loop;
    return Trim_Leading_Zeros (Codes);
  end;

  ------------
  -- Encode --
  ------------
  function Encode (Item : in Byte_Array) return Encoded_String is
    Leading_Ones : Encoded_String (1 .. Count_Leading_Zeros (Item)) := (others => '1');
    Codes        : Code_Type_Array := To_Code_Type_Array (Trim_Leading_Zeros (Item));
    Output       : Encoded_String (Codes'Range);
  begin
    for I in Codes'Range loop
      Output (I) := Encoded_Character'Val (Codes (I));
    end loop;
    return Leading_Ones & Output;
  end;

  ------------------------
  -- Count_Leading_Ones --
  ------------------------
  function Count_Leading_Ones (Items : in Encoded_String) return Natural is
    Output : Natural := 0;
  begin
    for I in Items'Range loop
      exit when Items (I) /= '1';
      Output := Natural'Succ (Output);
    end loop;
    return Output;
  end;

  -----------------------
  -- Trim_Leading_Ones --
  -----------------------
  function Trim_Leading_Ones (Items : in Encoded_String) return Encoded_String is (Items (Items'First + Count_Leading_Ones (Items) .. Items'Last));

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Item : in Encoded_String) return Byte_Array is
    Bytes         : Byte_Array (1 .. Item'Length * 733 /1000 + 1) := (others => 0);
    Bytes_Length  : Natural := 0;
  begin
    for Encoded of Item loop
      declare
        Decoded       : Code_Type   := Encoded_Character'Pos (Encoded);
        Carry         : Unsigned_16 := Unsigned_16 (Decoded);
        Bytes_Visited : Natural     := 0;
      begin
        for I in reverse Bytes'Range loop
          exit when (Carry = 0 and Bytes_Visited >= Bytes_Length);
          Carry := Carry + (58 * Unsigned_16 (Bytes (I)));
          Bytes (I) := Byte (Carry rem 256);
          Carry := Carry / 256;
          Bytes_Visited := Natural'Succ (Bytes_Visited);
        end loop;
        Bytes_Length := Bytes_Visited;
      end;
    end loop;
    return Trim_Leading_Zeros (Bytes);
  end;

  ------------
  -- Decode --
  ------------
  function Decode (Item : in Encoded_String) return Byte_Array is
    Leading_Zeros : Byte_Array (1 .. Count_Leading_Ones (Item)) := (others => 0);
    Bytes         : Byte_Array := To_Byte_Array ( Trim_Leading_Ones (Item));
  begin
    return Leading_Zeros & Bytes;
  end;

end;