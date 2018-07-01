package body Bitcoin.Data.Stacks is
  use Stack_Vectors;

  ----------
  -- Push --
  ----------
  procedure Push (Into : in out Stack_Type; Item : in Element_Type) is
  begin
   Append (Into, Item);
  end;

  ---------
  -- Pop --
  ---------
  function Pop (From : in out Stack_Type) return Element_Type is
    Output : Element_Type := From.Last_Element;
  begin
    Delete_Last (From);
    return Output;
  end;

  ---------
  -- Pop --
  ---------
  procedure Pop (From : in out Stack_Type) is
  begin
    if Size (From) = 0 then raise Constraint_Error; end if;
    Delete_Last (From);
  end;

  ----------
  -- Peek --
  ----------
  function Peek (Into : in Stack_Type) return Element_Type is (Last_Element (Into));

  ----------
  -- Swap --
  ----------
  procedure Swap (Stack : in out Stack_Type; Source : in Index_Type; Target : in Index_Type) is
    Target_Copy : Element_Type := Element (Stack, Target);
  begin
    Replace_Element (Stack, Target, Stack.Element(Source));
    Replace_Element (Stack, Source, Target_Copy);
  end;

  ------------
  -- Delete --
  ------------
  procedure Delete (From : in out Stack_Type; Index : in Index_Type) is begin
    Delete (From, Index, 1);
  end;

end;
