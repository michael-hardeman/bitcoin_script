package Bitcoin.Encoding.Base58_Characters is
  type Encoded_Character is (
    '1', '2', '3', '4', '5', '6', '7', '8',
    '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q',
    'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
    'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'i', 'j', 'k', 'm', 'n', 'o', 'p',
    'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z');

  function To_Character         (Item : in Encoded_Character) return Character;
  function To_Encoded_Character (Item : in Character)         return Encoded_Character;

  Malformed_Base58 : exception;
end;
