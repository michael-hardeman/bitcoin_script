package body Bitcoin.Encoding.Base58_Characters is

  ------------------
  -- To_Character --
  ------------------
  function To_Character (Item : in Encoded_Character) return Character is (
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
  function To_Encoded_Character (Item : in Character) return Encoded_Character is
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

end;
