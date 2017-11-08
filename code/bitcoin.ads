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
  function Image (Items : Byte_Array) return String;
  function To_Byte_Array (Item : in String) return Byte_Array;

end;
