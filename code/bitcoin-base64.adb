package body Bitcoin.Base64 is

  ------------------
  -- To_Character --
  ------------------
  function To_Character (Item : Encoded_Character) return Character is (
    case Item is
      when '0'..'9' => Character'Val (48 + (Encoded_Character'Pos (Item) - 52)),
      when 'A'..'Z' => Character'Val (65 + (Encoded_Character'Pos (Item)     )),
      when 'a'..'z' => Character'Val (97 + (Encoded_Character'Pos (Item) - 26)),
      when '+' => '+', when '/' => '/', when '=' => '=');

  --------------------------
  -- To_Encoded_Character --
  --------------------------
  function To_Encoded_Character (Item : Character) return Encoded_Character is
  begin
    case Item is
      when '0'..'9' => return Encoded_Character'Val (52 + (Character'Pos (Item) - 48));
      when 'A'..'Z' => return Encoded_Character'Val (     (Character'Pos (Item) - 65));
      when 'a'..'z' => return Encoded_Character'Val (26 + (Character'Pos (Item) - 97));
      when '+'      => return '+';
      when '/'      => return '/';
      when '='      => return '=';
      when others   => raise Malformed_Base64;
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

  ------------
  -- Encode --
  ------------
  function Encode (Item : in Byte_Array) return Encoded_String is
    Main_Length : constant Natural := Item'Length / 3;
    Remainder   : constant Natural := Item'Length rem 3;
    Output      : Encoded_String (1 .. (4 * (((Item'Length - 1) / 3) + 1)));
    Output_I    : Positive := Output'First;
  begin
    for I in 0 .. Main_Length - 1 loop
      Output (Output_I    ) := Encoded_Character'Val (Shift_Right ((Item (I * 3 + 1)       ), 2));
      Output (Output_I + 1) := Encoded_Character'Val (Shift_Left  ((Item (I * 3 + 1) and  3), 4) + Shift_Right (Item (I * 3 + 2), 4));
      Output (Output_I + 2) := Encoded_Character'Val (Shift_Left  ((Item (I * 3 + 2) and 15), 2) + Shift_Right (Item (I * 3 + 3), 6));
      Output (Output_I + 3) := Encoded_Character'Val (Item (I * 3 + 3) and 63);
      Output_I := Output_I + 4;
    end loop;
    case Remainder is
      when 2 =>
        Output (Output_I    ) := Encoded_Character'Val (Shift_Right ((Item (Item'Last - 1)      ), 2));
        Output (Output_I + 1) := Encoded_Character'Val (Shift_Left  ((Item (Item'Last - 1) and 3), 4) + Shift_Right (Item (Item'Last), 4));
        Output (Output_I + 2) := Encoded_Character'Val (Shift_Left  ((Item (Item'Last) and 15), 2));
        Output (Output_I + 3) := '=';
        return Output;
      when 1 =>
        Output (Output_I    ) := Encoded_Character'Val (Shift_Right (Item (Item'Last), 2));
        Output (Output_I + 1) := Encoded_Character'Val (Shift_Left  ((Item (Item'Last) and 3), 4));
        Output (Output_I + 2) := '=';
        Output (Output_I + 3) := '=';
        return Output;
      when others => return Output;
    end case;
  end;

  ------------
  -- Decode --
  ------------
  function Decode (Item : Encoded_String) return Byte_Array is
    Main_Length : constant Natural := Item'Length / 4;
    Output      : Byte_Array (1 .. ((Item'Length / 4) * 3)) := (others => 0);
    Output_I    : Positive := Output'First;
  begin
    for I in 0 .. Main_Length - 2 loop
      Output (Output_I    ) := Shift_Left (Encoded_Character'Pos (Item (I * 4 + 1)), 2) + Shift_Right (Encoded_Character'Pos (Item (I * 4 + 2)), 4);
      Output (Output_I + 1) := Shift_Left (Encoded_Character'Pos (Item (I * 4 + 2)), 4) + Shift_Right (Encoded_Character'Pos (Item (I * 4 + 3)), 2);
      Output (Output_I + 2) := Shift_Left (Encoded_Character'Pos (Item (I * 4 + 3)), 6) + Encoded_Character'Pos (Item (I * 4 + 4));
      Output_I := Output_I + 3;
    end loop;
    if Item (Item'Last - 1) = '=' then
      Output (Output_I) := Shift_Left (Encoded_Character'Pos (Item (Item'Last - 3)), 2) + Shift_Right (Encoded_Character'Pos (Item (Item'Last - 2)), 4);
      return Output (1 .. Output'Last - 2);
    elsif Item (Item'Last) = '=' then
      Output (Output_I    ) := Shift_Left (Encoded_Character'Pos (Item (Item'Last - 3)), 2) + Shift_Right (Encoded_Character'Pos (Item (Item'Last - 2)), 4);
      Output (Output_I + 1) := Shift_Left (Encoded_Character'Pos (Item (Item'Last - 2)), 4) + Shift_Right (Encoded_Character'Pos (Item (Item'Last - 1)), 2);
      return Output (1 .. Output'Last - 1);
    else
      Output (Output_I    ) := Shift_Left (Encoded_Character'Pos (Item (Item'Last - 3)), 2) + Shift_Right (Encoded_Character'Pos (Item (Item'Last - 2)), 4);
      Output (Output_I + 1) := Shift_Left (Encoded_Character'Pos (Item (Item'Last - 2)), 4) + Shift_Right (Encoded_Character'Pos (Item (Item'Last - 1)), 2);
      Output (Output_I + 2) := Shift_Left (Encoded_Character'Pos (Item (Item'Last - 1)), 6) + Encoded_Character'Pos (Item (Item'Last));
      return Output;
    end if;
  end;
end;