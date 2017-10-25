with Ada.Text_IO; use Ada.Text_IO;

package body Bitcoin.Crypto is

  -------------------
  -- Ignore/Assert --
  -------------------
  procedure Ignore (Result : in Address) is begin null; end;
  procedure Ignore (Result : in Int)     is begin null; end;
  procedure Assert (Result : in Int)     is begin
    if Result /= 1 then
      declare
        Error_Message : chars_ptr := New_Char_Array ((0 .. 255 => nul));
      begin
        ERR_error_string (ERR_get_error, Error_Message);
        raise Assertion_Failed with Value (Error_Message);
      end;
    end if;
  end;

  -----------------
  -- Conversions --
  -----------------
  function To_Int is new Ada.Unchecked_Conversion (Source => Curve_Kind,        Target => Int);
  function To_Int is new Ada.Unchecked_Conversion (Source => Point_Format_Kind, Target => Int);

  -----------------------------
  -- Controlled Access Types --
  -----------------------------
  procedure Initialize (Item : in out Key_Pair_Type; Curve : in Curve_Kind) is begin Item.Ptr := EC_KEY_new_by_curve_name (To_Int (Curve)); end;
  procedure Finalize   (Item : in out Key_Pair_Type)                        is begin EC_KEY_free (Item.Ptr);                                end;

  procedure Initialize (Item : in out Big_Number_Type) is begin Item.Ptr := BN_new;       end;
  procedure Finalize   (Item : in out Big_Number_Type) is begin BN_clear_free (Item.Ptr); end;

  procedure Initialize (Item : in out Big_Number_Context) is begin Item.Ptr := BN_CTX_new; BN_CTX_start (Item.Ptr); end;
  procedure Finalize   (Item : in out Big_Number_Context) is begin BN_CTX_end (Item.Ptr);  BN_CTX_free  (Item.Ptr); end;

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
    Ignore (BN_bn2bin (Item, Output'Address));
    return Output;
  end;

  -----------------------
  -- Derive_Public_Key --
  -----------------------
  procedure Derive_Public_Key (Key_Pair : in out Key_Pair_Type; Private_Key : in Byte_Array) is
    Priv_Key : BIGNUM_Access := new BIGNUM'(Null_Address, 0, 0, 0, 0);
    Group    : EC_GROUP      := EC_KEY_get0_group (Key_Pair.Ptr);
    Pub_Key  : EC_POINT      := EC_POINT_new (Group);
  begin
    Put_Line ("1");
    Ignore (BN_bin2bn (Private_Key'Address, Private_Key'Length, Priv_Key'Address));
    Put_Line ("2");
    Assert (EC_KEY_set_private_key (Key_Pair.Ptr, Priv_Key'Address));
    Put_Line ("3");
    declare Context : Big_Number_Context; begin
      Put_Line ("4");
      Assert (EC_POINT_mul (Group, Pub_Key, Priv_Key, Null_Address, Null_Address, Context.Ptr));
      Put_Line ("5");
      Assert (EC_KEY_set_public_key (Key_Pair.Ptr, Pub_Key));
    end;

  end;

  ---------------------
  -- Get_Private_Key --
  ---------------------
  function Get_Private_Key (Key_Pair : in Key_Pair_Type) return Byte_Array is
    Private_Key : BIGNUM := EC_KEY_get0_private_key (Key_Pair.Ptr);
    Output      : Byte_Array (1 .. Positive (BN_num_bytes (Private_Key))) := (others => 0);
  begin
    return To_Byte_Array (Private_Key);
  end;

  --------------------
  -- Get_Public_Key --
  --------------------
  function Get_Public_Key (Key_Pair : in Key_Pair_Type; Format : in Point_Format_Kind) return Byte_Array is
    Point      : EC_POINT := EC_KEY_get0_public_key (Key_Pair.Ptr);
    Group      : EC_GROUP := EC_KEY_get0_group (Key_Pair.Ptr);
    Public_Key : BIGNUM;
    Context    : Big_Number_Context;
  begin
    Ignore (EC_POINT_point2bn (Group, Point, To_Int (Format), Public_Key, Context.Ptr));
    return To_Byte_Array (Public_Key);
  end;

end;
