package body Bitcoin is

  -----------
  -- Image --
  -----------
  function Image (Items : Byte_Array) return String is
    Output : Unbounded_String;
  begin
    Append (Output, "(");
    for I in Items'First .. Items'Last - 1 loop
      Append (Output, Byte'Image (Items (I))); Append (Output, ",");
    end loop;
    Append (Output, Byte'Image (Items (Items'Last))); Append (Output, " )");
    return To_String (Output);
  end;

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Item : in String) return Byte_Array is
    Output : Byte_Array (Item'Range) := (others => 0);
  begin
    for I in Item'Range loop Output (I) := Byte (Character'Pos (Item (I))); end loop;
    return Output;
  end;

end;
