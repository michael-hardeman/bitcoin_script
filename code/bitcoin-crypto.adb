package body Bitcoin.Crypto is

  -------------------
  -- Ignore/Assert --
  -------------------
  procedure Ignore (Result : in Int) is begin null; end;
  procedure Assert (Result : in Int) is begin
    if Result = 1 then return; end if;
    declare
      Error_Message : Chars_Ptr := New_Char_Array ((0 .. 255 => nul));
    begin
      ERR_error_string (ERR_get_error, Error_Message);
      raise Assertion_Failed with Value (Error_Message);
    end;
  end;

  -----------------
  -- Conversions --
  -----------------
  function To_Unsigned is new Ada.Unchecked_Conversion (Source => Curve_Kind,        Target => Unsigned);
  function To_Unsigned is new Ada.Unchecked_Conversion (Source => Point_Format_Kind, Target => Unsigned);

  ----------
  -- Free --
  ----------
  procedure Free (Item : in Allocation_State) is
  begin
    case Item.Kind is
      when BIGNUM_Kind   => BN_clear_free       (Item.Ptr);
      when EC_POINT_Kind => EC_POINT_clear_free (Item.Ptr);
      when EC_GROUP_Kind => EC_GROUP_clear_free (Item.Ptr);
    end case;
  end;

  --------------
  -- Allocate --
  --------------
  procedure Allocate (Item : in out Key_Pair_Type; Kind : Allocation_Kind; Ptr : in Address) is
  begin
    Append (Item.Allocated, Allocation_State'(Kind, Ptr));
  end;

  --------------------
  -- Free_Allocated --
  --------------------
  procedure Free_Allocated (Item : in out Key_Pair_Type) is
  begin
    -- Put_Line ("Freeing EC_KEY: " & Address_Image (Item.Ptr));
    for Allocated of Item.Allocated loop
      -- Put_Line ("  " & Allocation_Kind'Image (Allocated.Kind) & ": " & Address_Image (Allocated.Ptr));
      Free (Allocated);
    end loop;
    Clear (Item.Allocated);
  end;

  -------------------
  -- Key_Pair_Type --
  -------------------
  procedure Initialize (Item : in out Key_Pair_Type; Curve : in Curve_Kind) is begin Item.Ptr := EC_KEY_new_by_curve_name (To_Unsigned (Curve)); end;
  procedure Finalize   (Item : in out Key_Pair_Type)                        is begin Free_Allocated (Item); EC_KEY_free (Item.Ptr);              end;

  ------------------------
  -- Big_Number_Context --
  ------------------------
  procedure Initialize (Item : in out Big_Number_Context) is begin Item.Ptr := BN_CTX_secure_new; BN_CTX_start (Item.Ptr); end;
  procedure Finalize   (Item : in out Big_Number_Context) is begin BN_CTX_end (Item.Ptr); BN_CTX_free (Item.Ptr);          end;

  -----------------------
  -- Generate_Key_Pair --
  -----------------------
  procedure Generate_Key_Pair (Key_Pair : in out Key_Pair_Type) is begin Assert (EC_KEY_generate_key (Key_Pair.Ptr)); end;

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Item : in BIGNUM) return Byte_Array is
    Output : Byte_Array (1 .. Positive (BN_num_bytes (Item))) := (others => 0);
  begin
    Ignore (BN_bn2bin (Item, Output (Output'First)'Unchecked_Access));
    return Output;
  end;

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Key_Pair : in Key_Pair_Type; Item : in ECDSA_SIG) return Byte_ARRAY is
    Output : Byte_Array (1 .. Positive (ECDSA_size (Key_Pair.Ptr))) := (others => 0);
    Length : Positive := i2d_ECDSA_SIG (Item, Output (Output'First)'Unchecked_Access);
  begin
    return Output (1 .. Length);
  end;

  ---------------
  -- To_BIGNUM --
  ---------------
  function To_BIGNUM (Item : in out Byte_Array) return BIGNUM is
    (BN_bin2bn (Item (Item'First)'Unchecked_Access, Item'Length, NULL_ADDRESS));

  -----------------------
  -- Derive_Public_Key --
  -----------------------
  procedure Derive_Public_Key (Key_Pair : in out Key_Pair_Type; Private_Key : in out Byte_Array) is
    Private_BIGNUM  : BIGNUM   := To_BIGNUM (Private_Key);
    Group           : EC_GROUP := EC_KEY_get0_group (Key_Pair.Ptr);
    Public_EC_POINT : EC_POINT := EC_POINT_new (Group);
  begin
    Allocate (Key_Pair, BIGNUM_Kind, Private_BIGNUM);
    Allocate (Key_Pair, EC_POINT_Kind, Public_EC_POINT);
    EC_KEY_set_asn1_flag (Key_Pair.Ptr, EC_NAMED_CURVE);
    Assert (EC_KEY_set_private_key (Key_Pair.Ptr, Private_BIGNUM));
    declare
      Context : Big_Number_Context;
    begin
      Assert (EC_POINT_mul (Group, Public_EC_POINT, Private_BIGNUM, NULL_ADDRESS, NULL_ADDRESS, Context.Ptr));
      Assert (EC_KEY_set_public_key (Key_Pair.Ptr, Public_EC_POINT));
    end;
  end;

  ---------------------
  -- Get_Private_Key --
  ---------------------
  function Get_Private_Key (Key_Pair : in Key_Pair_Type)
    return Byte_Array is (To_Byte_Array (EC_KEY_get0_private_key (Key_Pair.Ptr)));

  --------------------
  -- Get_Public_Key --
  --------------------
  function Get_Public_Key (Key_Pair : in Key_Pair_Type; Format : in Point_Format_Kind) return Byte_Array is
    Context       : Big_Number_Context;
    Public_BIGNUM : BIGNUM := EC_POINT_point2bn (
      EC_KEY_get0_group (Key_Pair.Ptr),
      EC_KEY_get0_public_key (Key_Pair.Ptr),
      To_Unsigned (Format),
      Null_Address,
      Context.Ptr);
    Output : Byte_Array := To_Byte_Array (Public_BIGNUM);
  begin
    BN_clear_free (Public_BIGNUM);
    return Output;
  end;

  ------------------
  -- Sign_Message --
  ------------------
  -- Does not require the Key_Pair public key to be set.
  function Sign_Message (Key_Pair : in Key_Pair_Type; Message : in out Byte_Array) return Byte_Array is
    Signature : ECDSA_SIG  := ECDSA_do_sign (Message (Message'First)'Unchecked_Access, Message'Length, Key_Pair.Ptr);
    Output    : Byte_Array := To_Byte_Array (Key_Pair, Signature);
  begin
    ECDSA_SIG_free (Signature);
    return Output;
  end;

  ---------------------------
  -- Verify_Signed_Message --
  ---------------------------
  -- Does not require the Key_Pair private key to be set.
  function Verify_Signed_Message (Key_Pair : in Key_Pair_Type; Signature : in out Byte_Array; Message : in out Byte_Array) return Boolean is
    Signature_C : ECDSA_SIG := d2i_ECDSA_SIG (NULL_ADDRESS, Signature (Signature'First)'Unchecked_Access, Signature'Length);
    Output      : Int       := ECDSA_do_verify (Message (Message'First)'Unchecked_Access, Message'Length, Signature_C, Key_Pair.Ptr);
  begin
    ECDSA_SIG_free (Signature_C);
    return (1 = Output);
  end;

end;
