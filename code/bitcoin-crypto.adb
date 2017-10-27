with System.Address_Image;

package body Bitcoin.Crypto is

  -------------------
  -- Ignore/Assert --
  -------------------
  end;

  -----------------
  -- Conversions --
  -----------------

  -----------------------------
  -- Controlled Access Types --
  -----------------------------
  procedure Initialize (Item : in out Key_Pair_Type; Curve : in Curve_Kind) is begin Item.Ptr := EC_KEY_new_by_curve_name (To_Int (Curve)); end;
  procedure Finalize   (Item : in out Key_Pair_Type)                        is begin EC_KEY_free (Item.Ptr);                                end;
  -------------------

  procedure Initialize (Item : in out Big_Number_Context) is begin Item.Ptr := BN_CTX_new; BN_CTX_start (Item.Ptr); end;
  procedure Finalize   (Item : in out Big_Number_Context) is begin BN_CTX_end (Item.Ptr);  BN_CTX_free  (Item.Ptr); end;

  -----------------------
  -- Generate_Key_Pair --
  -----------------------
  procedure Generate_Key_Pair (Key_Pair : in out Key_Pair_Type) is begin Assert (EC_KEY_generate_key (Key_Pair.Ptr)); end;

  -------------------
  -- To_Byte_Array --
  -------------------
    Output : Byte_Array (1 .. Positive (BN_num_bytes (Item))) := (others => 0);
  begin
    Ignore (BN_bn2bin (Item, Output (Output'First)'Unchecked_Access));
    return Output;
  end;


  -----------------------
  -- Derive_Public_Key --
  -----------------------
  procedure Derive_Public_Key (Key_Pair : in out Key_Pair_Type; Private_Key : in out Byte_Array) is
  begin
    end;
  end;

  ---------------------
  -- Get_Private_Key --
  ---------------------

  --------------------
  -- Get_Public_Key --
  --------------------
  function Get_Public_Key (Key_Pair : in Key_Pair_Type; Format : in Point_Format_Kind) return Byte_Array is
  begin
  end;

end;
