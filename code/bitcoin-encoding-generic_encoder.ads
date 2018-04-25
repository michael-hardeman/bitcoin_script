
-- This Generic_Encoder can be used to encode data in any base you want up to 
-- base 255. The base is determined by the Encoded_Character Range.
-- If you give it 10 characters then it will encode in base 10 with those 
-- characters.

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

generic
  type Encoded_Character is (<>);
  with function To_Character         (Item : in Encoded_Character) return Character;
  with function To_Encoded_Character (Item : in Character)         return Encoded_Character;
package Bitcoin.Encoding.Generic_Encoder is
  
  -----------
  -- Types --
  -----------
  type Encoded_String is array (Positive range <>) of Encoded_Character;
  
  -----------------
  -- Subprograms --
  -----------------
  function Encode            (Item : in Byte_Array)     return Encoded_String;
  function Decode            (Item : in Encoded_String) return Byte_Array;
  function To_String         (Item : in Encoded_String) return String;
  function To_Encoded_String (Item : in String)         return Encoded_String;
  
-------
private
-------
  
  -----------
  -- Types --
  -----------
  subtype Code_Type is Unsigned_8 range 0 .. Unsigned_8'Min (Unsigned_8'Last, Encoded_Character'Pos (Encoded_Character'Last));
  type Code_Type_Array is array (Positive range <>) of Code_Type;
  
  ---------------
  -- Constants --
  ---------------
  BASE                       : constant Unsigned_16 := Unsigned_16(Code_Type'Last) + 1;
  BITS_PER_ENCODED_CHARACTER : constant Float       := Log (X => Float (BASE), Base => 2.0);
  BITS_PER_BYTE              : constant Float       := 8.0;
  
  -----------------
  -- Subprograms --
  -----------------
  -- encode convert binary to base N = log2 (N)
  function Compute_Encoding_Length (Input_Length : in Positive) return Positive is (   
    Positive (Float'Ceiling (Float (Input_Length) * BITS_PER_BYTE / BITS_PER_ENCODED_CHARACTER)));
  -- decode convert base N to binary = logN (2)
  function Compute_Decoding_Length (Input_Length : in Positive) return Positive is (
    Positive (Float'Ceiling (Float (Input_Length) * BITS_PER_ENCODED_CHARACTER / BITS_PER_BYTE)));
  
  --------------
  -- Packages --
  --------------
  generic
    type Element_Type is (<>);
    type Index_Type is range <>;
    type Array_Type is array (Index_Type range <>) of Element_Type;
  package Array_Operations is
    function Count_Leading  (Items : in Array_Type; Value : in Element_Type)                     return Natural;
    function Trim_Leading   (Items : in Array_Type; Value : in Element_Type)                     return Array_Type;
    function Append_Leading (Items : in Array_Type; Value : in Element_Type; Count : in Natural) return Array_Type;
  end;
  
end Bitcoin.Encoding.Generic_Encoder;
