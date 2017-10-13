with Interfaces; use Interfaces;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package Bitcoin.Base64 is

  type Encoded_Character is (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8' ,'9', '+', '/', '=');
  type Encoded_String is array (Positive range <>) of Encoded_Character;

  -- To_Encoded_String will raise the following exception when given invalid base64
  -- Ensure the String is Trimed and any invalid characters are removed.
  Malformed_Base64 : exception;
  function To_Encoded_String (Item : in String)         return Encoded_String;
  function To_String         (Item : in Encoded_String) return String;

  function Encode (Item : in Byte_Array)     return Encoded_String;
  function Decode (Item : in Encoded_String) return Byte_Array;
end;