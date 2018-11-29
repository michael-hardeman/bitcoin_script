with Interfaces; use Interfaces;
with System.Bignums; use System.Bignums;
with Ada.Unchecked_Deallocation;

package Bitcoin is

  -----------
  -- Types --
  -----------
  subtype Byte is Unsigned_8;
  type Byte_Access is access all Byte;
  type Byte_Array is array (Positive range <>) of aliased Byte;

  -----------------
  -- Subprograms --
  -----------------
  -- prints the byte array in the form: ( byte, byte, byte ... )
  function Image (Bytes : in Byte_Array) return String;
  -- converts the byte array back to character codes
  function To_String (Bytes : in Byte_Array) return String;
  -- converts characters to their 'Pos
  function To_Byte_Array (Item  : in String) return Byte_Array;

  function Count_Leading_Zeros (Bytes : in Byte_Array) return Natural;
  function Trim_Leading_Zeros  (Bytes : in Byte_Array) return Byte_Array;
  function Is_Zero (Bytes : in Byte_Array) return Boolean is (Bytes = (Bytes'Range => 16#00#));
  function Is_One  (Bytes : in Byte_Array) return Boolean;

  -- If Bytes'Length > 4 then it must be beyond Natural'Last.
  -- If Bytes'Length = 4 and it is greater than (16#7F#, 16#FF#, 16#FF#, 16#FF#); then
  -- it is a two's compliment negative number which is also a constraint error.
  function To_Natural (Bytes : in Byte_Array) return Natural;

  -- Will remove leading 0's since the underlying library does not support them.
  -- Converts the given binary data into a System.Bignums.Bignum
  -- Requires the Bignum must be deallocated after
  function To_Bignum (Bytes : in Byte_Array) return Bignum;

  -- Converts a System.Bignums.Bignum into a Byte_Array
  -- Will remove any leading 0's in the final result
  function To_Byte_Array (Item : in Bignum) return Byte_Array;
  procedure Free_Bignum is new Ada.Unchecked_Deallocation (Object => Bignum_Data, Name => Bignum);

end;
