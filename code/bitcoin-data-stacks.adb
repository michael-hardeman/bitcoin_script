package body Bitcoin.Data.Stacks is
  use Stack_Indefinite_Vectors;

  ----------
  -- Push --
  ----------
  procedure Push (To : in out Stack_Type; Item : in Element_Type) is
  begin
   To.Append (Item);
  end;

  ---------
  -- Pop --
  ---------
  function Pop (From : in out Stack_Type) return Element_Type is
    Output : Element_Type := From.Last_Element;
  begin
    From.Delete_Last;
    return Output;
  end;

  ---------
  -- Pop --
  ---------
  procedure Pop (From : in out Stack_Type) is
  begin
    From.Delete_Last;
  end;

  ----------
  -- Peek --
  ----------
  function Peek (Into : in Stack_Type) return Element_Type is (Into.Last_Element);

end;
