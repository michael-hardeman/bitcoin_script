with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Vectors;

generic
  type Index_Type is range <>;
  type Element_Type (<>) is private;
  with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Generic_Stacks is

  package Stack_Indefinite_Vectors is new Ada.Containers.Indefinite_Vectors (
    Index_Type   => Index_Type,
    Element_Type => Element_Type,
    "="          => "=");

  subtype Stack_Type is Stack_Indefinite_Vectors.Vector;

  function  Peek (Into : in     Stack_Type) return Element_Type;
  procedure Push (To   : in out Stack_Type; Item : in Element_Type);
  function  Pop  (From : in out Stack_Type) return Element_Type;
  procedure Pop  (From : in out Stack_Type);
end;
