with Interfaces; use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
  function Image         (Bytes : in Byte_Array) return String;
  function To_Byte_Array (Item  : in String)     return Byte_Array;
  function To_Natural    (Bytes : in Byte_Array) return Natural;
  function Is_Zero       (Bytes : in Byte_Array) return Boolean is (Bytes = (Bytes'Range => 16#00#));
  function Is_One        (Bytes : in Byte_Array) return Boolean;

end;
