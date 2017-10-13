with Ada.Finalization;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package OpenSSL.Crypto.Thick is

  -----------
  -- Types --
  -----------
  type Key_Pair_Type   is new Ada.Finalization.Controlled with private;
  type Big_Number_Type is new Ada.Finalization.Controlled with private;

  ------------------
  -- Enumerations --
  ------------------
  type Elliptical_Curve_Kind is (secp256k1) with Size => Int'Size;
  for Elliptical_Curve_Kind use (secp256k1 => NID_secp256k1);

  type Point_Format_Kind is (Compressed, Ucompressed, Hybrid) with Size => Int'Size;
  for Point_Format_Kind use (
    Compressed  => POINT_CONVERSION_COMPRESSED,
    Ucompressed => POINT_CONVERSION_UNCOMPRESSED,
    Hybrid      => POINT_CONVERSION_HYBRID);

  type Base64_BIO_Flag_Kind is (No_New_Line) with Size => Int'Size;
  for Base64_BIO_Flag_Kind use (No_New_Line => BIO_FLAGS_BASE64_NO_NL);

  -----------------
  -- Subprograms --
  -----------------
  procedure Generate_Key_Pair (Key_Pair : in out Key_Pair_Type);
  procedure Derive_Public_Key (Key_Pair : in out Key_Pair_Type; Private_Key : in Byte_Array);

  procedure Get_Private_Key (Private_Key : in out Big_Number_Type'Class; Key_Pair : in Key_Pair_Type);
  procedure Get_Public_Key  (Public_Key  : in out Big_Number_Type'Class; Key_Pair : in Key_Pair_Type; Format : in Point_Format_Kind);

  function  Length        (Big_Number : in Big_Number_Type) return Positive;
  procedure To_Byte_Array (Big_Number : in Big_Number_Type; Output : in out Byte_Array);
  
-------
private
-------

  -- I'm wrapping all these C allocated access types to prevent memory leaks
  type Key_Pair_Type is new Ada.Finalization.Controlled with record
    Ptr : EC_KEY := Null_Address;
  end record;
  overriding procedure Initialize (Item : in out Key_Pair_Type);
  overriding procedure Finalize   (Item : in out Key_Pair_Type);

  type Big_Number_Type is new Ada.Finalization.Controlled with record
    Ptr    : BIGNUM   := Null_Address;
    Length : Positive := 1;
  end record;
  overriding procedure Initialize (Item : in out Big_Number_Type);
  overriding procedure Finalize   (Item : in out Big_Number_Type);

  -- Declaring this variable automatically calls BN_CTX_start
  -- When the variable goes out of scope it automatically calls BN_CTX_end
  -- These functions are used to obtain temporary BIGNUM variables from a
  -- BN_CTX (which can been created by using BN_CTX_new(3)) in order
  -- to save the overhead of repeatedly creating and freeing
  -- BIGNUMs in functions that are called from inside a loop.
  -- https://wiki.openssl.org/index.php/Manual:BN_CTX_start(3)
  type Big_Number_Context is new Ada.Finalization.Controlled with record
    Ptr : BN_CTX := Null_Address;
  end record;
  overriding procedure Initialize (Item : in out Big_Number_Context);
  overriding procedure Finalize   (Item : in out Big_Number_Context);

  type Elliptical_Curve_Point is new Ada.Finalization.Controlled with record
    Ptr : EC_Point := Null_Address;
  end record;
             procedure Initialize (Item : in out Elliptical_Curve_Point; Group : in EC_Group);
  overriding procedure Finalize   (Item : in out Elliptical_Curve_Point);

  type Binary_IO_Type is new Ada.Finalization.Controlled with record
    Ptr : BIO := Null_Address;
  end record;
             procedure Initialize (Item : in out Binary_IO_Type; Method : BIO_METHOD);
  overriding procedure Finalize   (Item : in out Binary_IO_Type);

  procedure Assert (Result : in Int);
  procedure Ignore (Result : in Int);

end;
