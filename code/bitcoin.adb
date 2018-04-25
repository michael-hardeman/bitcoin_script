package body Bitcoin is

  -----------
  -- Image --
  -----------
  function Image (Bytes : in Byte_Array) return String is
    Output : Unbounded_String;
  begin
    Append (Output, "(");
    for I in Bytes'First .. Bytes'Last - 1 loop
      Append (Output, Byte'Image (Bytes (I))); Append (Output, ",");
    end loop;
    Append (Output, Byte'Image (Bytes (Bytes'Last))); Append (Output, " )");
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

  ----------------
  -- To_Natural --
  ----------------
  function To_Natural (Bytes : in Byte_Array) return Natural is
    Output : Natural := 0;
  begin
    if Bytes'Length > 4 then raise Constraint_Error with "Input larger than Natural'Last"; end if;
    if Bytes'Length = 4 and then Bytes (Bytes'First) > 16#7F# and then
       (for all I in Bytes'First + 1 .. Bytes'Last => Bytes (I) = 16#FF#) then
       raise Constraint_Error with "Input is a large negative number Natural'Last";
    end if;

    for I in 0 .. Bytes'Last - 1 loop
      Output := Output + (Natural(Bytes (Bytes'Last - I)) * (2 ** (8 * I)));
    end loop;

    return Output;
  end;

  ------------
  -- Is_One --
  ------------
  function Is_One (Bytes : in Byte_Array) return Boolean is begin
    return (for all I in Bytes'First .. Bytes'Last => Bytes (I) = 16#00#) and then Bytes (Bytes'Last) = 16#01#;
  end;

  -------
  -- + --
  -------
  function "+"   (X, Y : Byte_Array) return Byte_Array is
    Output : Byte_Array (1 .. 1) := (others => Byte'First);
  begin
    return Output;
  end;

  -------
  -- - --
  -------
  function "-"   (X, Y : Byte_Array) return Byte_Array is
    Output : Byte_Array (1 .. 1) := (others => Byte'First);
  begin
    return Output;
  end;

  -------
  -- * --
  -------
  function "*"   (X, Y : Byte_Array) return Byte_Array is
    Output : Byte_Array (1 .. 1) := (others => Byte'First);
  begin
    return Output;
  end;

  -------
  -- / --
  -------
  function "/"   (X, Y : Byte_Array) return Byte_Array is
    Output : Byte_Array (1 .. 1) := (others => Byte'First);
  begin
    return Output;
  end;

  ---------
  -- mod --
  ---------
  function "mod" (X, Y : Byte_Array) return Byte_Array is
    Output : Byte_Array (1 .. 1) := (others => Byte'First);
  begin
    return Output;
  end;

  ---------
  -- rem --
  ---------
  function "rem" (X, Y : Byte_Array) return Byte_Array is
    Output : Byte_Array (1 .. 1) := (others => Byte'First);
  begin
    return Output;
  end;

  --------
  -- ** --
  --------
  function "**"  (X : Byte_Array; Exp : Natural) return Byte_Array is
    Output : Byte_Array (1 .. 1) := (others => Byte'First);
  begin
    return Output;
  end;

end;
