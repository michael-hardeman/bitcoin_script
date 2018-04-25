with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Vectors;

generic
  type Index_Type is range <>;
  type Element_Type (<>) is private;
  with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Bitcoin.Data.Stacks is

  package Stack_Vectors is new Ada.Containers.Indefinite_Vectors (Index_Type, Element_Type, "=");

  subtype Stack_Type is Stack_Vectors.Vector;

  function  Top_Index (Stack : in     Stack_Type) return Index_Type is (Stack_Vectors.Last_Index(Stack));
  function  Size      (Stack : in     Stack_Type) return Natural is (Natural (Stack_Vectors.Length (Stack)));
  function  Get       (Stack : in     Stack_Type; Index : in Index_Type) return Element_Type is (Stack_Vectors.Element (Stack, Index));
  function  Peek      (Into  : in     Stack_Type) return Element_Type;
  procedure Push      (Into  : in out Stack_Type; Item : in Element_Type);
  function  Pop       (From  : in out Stack_Type) return Element_Type;
  procedure Pop       (From  : in out Stack_Type);
  procedure Swap      (Stack : in out Stack_Type; Source : in Index_Type; Target : in Index_Type);
  procedure Delete    (From  : in out Stack_Type; Index  : in Index_Type);
end;
