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

  ---------------
  -- To_Bignum --
  ---------------
  -- Will remove leading 0's since the underlying library does not support them.
  -- Converts the given binary data into a System.Bignums.Bignum
  -- Requires the Bignum must be deallocated after

  function To_Bignum (Bytes : in Byte_Array) return Bignum
  is
    Trimmed        : Byte_Array := Trim_Leading_Zeros (Bytes);
    Whole_Words    : Natural    := Trimmed'Length / 4;
    Hanging_Bytes  : Natural    := Trimmed'Length rem 4;
    Trimmed_Offset : Positive   := 1;
    Output         : Bignum     := new Bignum_Data'(Len => Whole_Words + (if Hanging_Bytes = 0 then 0 else 1),
                                                    Neg => False,
                                                    D   => (others => 0));
  begin

    -- goes through the byte array backwards, combining all groups of 4 bytes into an Unsigned_32.

    for I in 0 .. Whole_Words - 1 loop
      Trimmed_Offset := Trimmed'Last - 4 * I;

      Output.D (Output.D'Last - I) :=             SD (Trimmed (Trimmed_Offset    ))
                                    + Shift_Left (SD (Trimmed (Trimmed_Offset - 1)),  8)
                                    + Shift_Left (SD (Trimmed (Trimmed_Offset - 2)), 16)
                                    + Shift_Left (SD (Trimmed (Trimmed_Offset - 3)), 24);
    end loop;

    -- Handles any leftover bytes, combining what's left into an Unsigned_32.

    if Hanging_Bytes = 0 then return Output; end if;

    Trimmed_Offset := Trimmed'Last - 4 * Whole_Words;

    if Hanging_Bytes >= 1 then
      Output.D (Output.D'First) := Output.D (Output.D'First) + SD (Trimmed (Trimmed_Offset));
    end if;

    if Hanging_Bytes >= 2 then
      Output.D (Output.D'First) := Output.D (Output.D'First) + Shift_Left (SD (Trimmed (Trimmed_Offset - 1)), 8);
    end if;

    if Hanging_Bytes  = 3 then
      Output.D (Output.D'First) := Output.D (Output.D'First) + Shift_Left (SD (Trimmed (Trimmed_Offset - 2)),  16);
    end if;

    return Output;
  end;


  -------------------
  -- To_Byte_Array --
  -------------------
  -- Converts a System.Bignums.Bignum into a Byte_Array
  -- Will remove any leading 0's in the final result

  function To_Byte_Array (Item : in Bignum) return Byte_Array is
    Bytes        : Byte_Array(1 .. Item.D'Length * 4) := (others => 0);
    Bytes_Offset : Natural;
  begin

    for I in 0 .. Item.D'Length - 1 loop
      Bytes_Offset := Bytes'First + 4 * I;

      Bytes (Bytes_Offset    ) := Byte (Shift_Right (Item.D (Item.D'First + I), 24)           );
      Bytes (Bytes_Offset + 1) := Byte (Shift_Right (Item.D (Item.D'First + I), 16) and 16#FF#);
      Bytes (Bytes_Offset + 2) := Byte (Shift_Right (Item.D (Item.D'First + I), 8)  and 16#FF#);
      Bytes (Bytes_Offset + 3) := Byte (             Item.D (Item.D'First + I)      and 16#FF#);
    end loop;

    if Bytes'Length = 0 then return Byte_Array'(1 => 16#00#); end if;

    return Trim_Leading_Zeros (Bytes);
  end;

end;
