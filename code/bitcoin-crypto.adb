package body Bitcoin.Crypto is

  ----------------
  -- C Wrappers --
  ----------------
  procedure Raise_OpenSSL_Exception is begin
    raise OpenSSL_Exception with Value (ERR_error_string (ERR_get_error, Null_Ptr));
  end;

  procedure Ignore (Result : in Int) is begin null; end;
  procedure Assert (Result : in Int) is begin
    if Result = 1 then return; end if;
    Raise_OpenSSL_Exception;
  end;
  procedure Assert_Not_Negative (Result : in Int) is begin
    if Result > -1 then return; end if;
    Raise_OpenSSL_Exception;
  end;

  -----------------
  -- To_Unsigned --
  -----------------
  function To_Unsigned is new Ada.Unchecked_Conversion (Source => Curve_Kind,        Target => Unsigned);
  function To_Unsigned is new Ada.Unchecked_Conversion (Source => Point_Format_Kind, Target => Unsigned);

  -------------------
  -- To_Byte_Array --
  -------------------
  function To_Byte_Array (Item : in BIGNUM) return Byte_Array is
    Output : Byte_Array (1 .. Positive (BN_num_bytes (Item))) := (others => 0);
  begin
    Ignore (BN_bn2bin (Item, Output (Output'First)'Unchecked_Access));
    return Output;
  end;

  ---------------
  -- To_BIGNUM --
  ---------------
  function To_BIGNUM (Item : in out Byte_Array) return BIGNUM is
    (BN_bin2bn (Item (Item'First)'Unchecked_Access, Item'Length, NULL_ADDRESS));

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
  procedure Allocate (
    Key_Pair : in out Key_Pair_Type;
    Kind     : in     Allocation_Kind;
    Ptr      : in     Address) is
  begin
    Append (Key_Pair.Allocated, Allocation_State'(Kind, Ptr));
  end;

  --------------------
  -- Free_Allocated --
  --------------------
  procedure Free_Allocated (Key_Pair : in out Key_Pair_Type) is
  begin
    -- Put_Line ("Freeing EC_KEY: " & Address_Image (Key_Pair.Ptr));
    for Allocated of Key_Pair.Allocated loop
      -- Put_Line ("  " & Allocation_Kind'Image (Allocated.Kind) & ": " & Address_Image (Allocated.Ptr));
      Free (Allocated);
    end loop;
    Clear (Key_Pair.Allocated);
  end;

  -----------------------
  -- Derive_Public_Key --
  -----------------------
  procedure Derive_Public_Key (
    Key_Pair    : in out Key_Pair_Type;
    Private_Key : in out Byte_Array)
  is
    Private_BIGNUM  : BIGNUM   := To_BIGNUM (Private_Key);
    Group           : EC_GROUP := EC_KEY_get0_group (Key_Pair.Low_Level_Ptr);
    Public_EC_POINT : EC_POINT := EC_POINT_new (Group);
  begin
    Allocate (Key_Pair, BIGNUM_Kind, Private_BIGNUM);
    Allocate (Key_Pair, EC_POINT_Kind, Public_EC_POINT);
    Assert (EC_KEY_set_private_key (Key_Pair.Low_Level_Ptr, Private_BIGNUM));
    declare
      Context : Big_Number_Context;
    begin
      Assert (EC_POINT_mul (Group, Public_EC_POINT, Private_BIGNUM, NULL_ADDRESS, NULL_ADDRESS, Context.Ptr));
      Assert (EC_KEY_set_public_key (Key_Pair.Low_Level_Ptr, Public_EC_POINT));
    end;
  end;

  --------------
  -- Generate --
  --------------
  procedure Generate (
    Key_Pair : in out Key_Pair_Type;
    Curve    : in Curve_Kind) is
  begin
    Key_Pair.Low_Level_Ptr := EC_KEY_new_by_curve_name (To_Unsigned (Curve));
    Key_Pair.Curve := Curve;
    Assert (EC_KEY_generate_key (Key_Pair.Low_Level_Ptr));
    Key_Pair.Abstracted_Ptr := EVP_PKEY_new;
    Assert (EVP_PKEY_assign_EC_KEY (Key_Pair.Abstracted_Ptr, Key_Pair.Low_Level_Ptr));
  end;

  ----------------------
  -- From_Private_Key --
  ----------------------
  procedure From_Private_Key (
    Key_Pair    : in out Key_Pair_Type;
    Curve       : in     Curve_Kind;
    Private_Key : in out Byte_Array) is
  begin
    Key_Pair.Low_Level_Ptr := EC_KEY_new_by_curve_name (To_Unsigned (Curve));
    Key_Pair.Curve := Curve;
    Derive_Public_Key (Key_Pair, Private_Key);
    Key_Pair.Abstracted_Ptr := EVP_PKEY_new;
    Assert (EVP_PKEY_assign_EC_KEY (Key_Pair.Abstracted_Ptr, Key_Pair.Low_Level_Ptr));
  end;

  --------------
  -- Finalize --
  --------------
  procedure Finalize (Key_Pair : in out Key_Pair_Type) is
  begin
    Free_Allocated (Key_Pair);
    EVP_PKEY_free  (Key_Pair.Abstracted_Ptr);
  end;

  ------------------------
  -- Big_Number_Context --
  ------------------------
  procedure Initialize (Item : in out Big_Number_Context) is
  begin
    Item.Ptr := BN_CTX_new;
    BN_CTX_start (Item.Ptr);
  end;

  procedure Finalize (Item : in out Big_Number_Context) is
  begin
    BN_CTX_end (Item.Ptr);
    BN_CTX_free (Item.Ptr);
  end;

  ---------------------
  -- Get_Private_Key --
  ---------------------
  function Get_Private_Key (
    Key_Pair : in Key_Pair_Type)
  return Byte_Array is (To_Byte_Array (EC_KEY_get0_private_key (Key_Pair.Low_Level_Ptr)));

  --------------------
  -- Get_Public_Key --
  --------------------
  function Get_Public_Key (
    Key_Pair : in Key_Pair_Type;
    Format   : in Point_Format_Kind)
  return Byte_Array is
    Context       : Big_Number_Context;
    Public_BIGNUM : BIGNUM := EC_POINT_point2bn (
      EC_KEY_get0_group (Key_Pair.Low_Level_Ptr),
      EC_KEY_get0_public_key (Key_Pair.Low_Level_Ptr),
      To_Unsigned (Format),
      Null_Address,
      Context.Ptr);
    Output : Byte_Array := To_Byte_Array (Public_BIGNUM);
  begin
    BN_clear_free (Public_BIGNUM);
    return Output;
  end;

  ----------
  -- Sign --
  ----------
  function Sign (
    Key_Pair    : in     Key_Pair_Type;
    Message     : in out Byte_Array)
  return Byte_Array is
    Key_Context      : EVP_PKEY_CTX := EVP_PKEY_CTX_new (Key_Pair.Abstracted_Ptr, NULL_ADDRESS);
    Signature_Length : Size_T;
  begin
    Assert (EVP_PKEY_sign_init (Key_Context));
    Assert (EVP_PKEY_CTX_set_signature_md (Key_Context, EVP_sha256));
    -- This call is to retrieve the maximum possible size of the signature
    -- Sometimes the signature will be 1 or 2 bytes smaller the return statement removes empty bytes
    Assert (
      EVP_PKEY_sign (
        Key_Context,
        null,
        Signature_Length'Address,
        Message (Message'First)'Unchecked_Access,
        Size_T (Message'Length)));
    declare Output : Byte_Array (1 .. Positive (Signature_Length)); begin
      Assert (
        EVP_PKEY_sign (
          Key_Context,
          Output (Output'First)'Unchecked_Access,
          Signature_Length'Address,
          Message (Message'First)'Unchecked_Access,
          Size_T (Message'Length)));
      EVP_PKEY_CTX_free (Key_Context);
      return Output (1 .. Positive(Signature_Length));
    end;
  end;

  ------------
  -- Verify --
  ------------
  -- Does not require the Key_Pair private key to be set.
  function Verify (
    Key_Pair  : in     Key_Pair_Type;
    Signature : in out Byte_Array;
    Message   : in out Byte_Array)
  return Boolean is
    Key_Context : EVP_PKEY_CTX := EVP_PKEY_CTX_new (Key_Pair.Abstracted_Ptr, NULL_ADDRESS);
    Output      : Int;
  begin
    Assert (EVP_PKEY_verify_init (Key_Context));
    Assert (EVP_PKEY_CTX_set_signature_md (Key_Context, EVP_sha256));
    Output := EVP_PKEY_verify (
      Key_Context,
      Signature (Signature'First)'Unchecked_Access,
      Size_T (Signature'Length),
      Message (Message'First)'Unchecked_Access,
      Size_T (Message'Length));
    EVP_PKEY_CTX_free (Key_Context);
    Assert_Not_Negative (Output);
    return (1 = Output);
  end;

end;
