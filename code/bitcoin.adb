with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
    if Bytes'Length > 0 then Append (Output, Byte'Image (Bytes (Bytes'Last))); end if;
    Append (Output, " )");
    return To_String (Output);
  end;

  ---------------
  -- To_String --
  ---------------
  function To_String (Bytes : in Byte_Array) return String is
    Output : String (Bytes'First .. Bytes'Last);
  begin
    for I in Bytes'Range loop
      Output (I) := Character'Val (Bytes (I));
    end loop;
    return Output;
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

  -------------------------
  -- Count_Leading_Zeros --
  -------------------------
  function Count_Leading_Zeros (Bytes : in Byte_Array) return Natural is
    Counter : Natural := 0;
  begin
    for I in Bytes'Range loop
      exit when Bytes(I) /= Byte'First;
      Counter := Natural'Succ (Counter);
    end loop;
    return Counter;
  end;

  ------------------------
  -- Trim_Leading_Zeros --
  ------------------------
  function Trim_Leading_Zeros (Bytes : in Byte_Array) return Byte_Array is (
    Bytes (Bytes'First + Count_Leading_Zeros (Bytes) .. Bytes'Last));

  ------------
  -- Is_One --
  ------------
  function Is_One (Bytes : in Byte_Array) return Boolean is
    Trimmed : Byte_Array := Trim_Leading_Zeros (Bytes);
  begin
    return (Trimmed'Length = 1 and then Trimmed (Trimmed'First) = 16#01#);
  end;

  ----------------
  -- To_Natural --
  ----------------
  function To_Natural (Bytes : in Byte_Array) return Natural is
    Output  : Natural    := 0;
    Trimmed : Byte_Array := Trim_Leading_Zeros (Bytes);
  begin
    if Is_Zero (Bytes) then return 0; end if;
    if Trimmed'Length > 4 then raise Constraint_Error with "Input larger than Natural'Last"; end if;
    if Trimmed'Length = 4 and then Bytes (Bytes'First) > 16#7F# and then
       (for all I in Trimmed'First + 1 .. Trimmed'Last => Trimmed (I) = 16#FF#) then
       raise Constraint_Error with "Input is a valid Integer, but is negative.";
    end if;

    for I in 0 .. Trimmed'Length - 1 loop
      Output := Output + (Natural(Trimmed (Trimmed'Last - I)) * (2 ** (8 * I)));
    end loop;

    return Output;
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
