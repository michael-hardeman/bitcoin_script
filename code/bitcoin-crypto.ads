with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Bitcoin.API.OpenSSL; use Bitcoin.API.OpenSSL;

package Bitcoin.Crypto is

  -----------
  -- Types --
  -----------
  type Key_Pair_Type               is new Ada.Finalization.Controlled with private;
  type Elliptical_Curve_Point_Type is new Ada.Finalization.Controlled with private;
  type Big_Number_Type             is new Ada.Finalization.Controlled with private;

  ------------------
  -- Enumerations --
  ------------------
  type Elliptical_Curve_Kind is (
    secp112r1, secp112r2, secp128r1, secp128r2, secp160k1, 
    secp160r1, secp160r2, secp192k1, secp224k1, secp224r1,
    secp256k1, secp384r1, secp521r1, sect113r1, sect113r2,
    sect131r1, sect131r2, sect163k1, sect163r1, sect163r2,
    sect193r1, sect193r2, sect233k1, sect233r1, sect239k1,
    sect283k1, sect283r1, sect409k1, sect409r1, sect571k1,
    sect571r1) with Size => Int'Size;
  for Elliptical_Curve_Kind use (
    secp112r1 => NID_secp112r1, secp112r2 => NID_secp112r2, secp128r1 => NID_secp128r1, secp128r2 => NID_secp128r2, secp160k1 => NID_secp160k1, 
    secp160r1 => NID_secp160r1, secp160r2 => NID_secp160r2, secp192k1 => NID_secp192k1, secp224k1 => NID_secp224k1, secp224r1 => NID_secp224r1,
    secp256k1 => NID_secp256k1, secp384r1 => NID_secp384r1, secp521r1 => NID_secp521r1, sect113r1 => NID_sect113r1, sect113r2 => NID_sect113r2,
    sect131r1 => NID_sect131r1, sect131r2 => NID_sect131r2, sect163k1 => NID_sect163k1, sect163r1 => NID_sect163r1, sect163r2 => NID_sect163r2,
    sect193r1 => NID_sect193r1, sect193r2 => NID_sect193r2, sect233k1 => NID_sect233k1, sect233r1 => NID_sect233r1, sect239k1 => NID_sect239k1,
    sect283k1 => NID_sect283k1, sect283r1 => NID_sect283r1, sect409k1 => NID_sect409k1, sect409r1 => NID_sect409r1, sect571k1 => NID_sect571k1,
    sect571r1 => NID_sect571r1);

  type Point_Format_Kind is (Compressed, Ucompressed, Hybrid) with Size => Int'Size;
  for Point_Format_Kind use (
    Compressed  => POINT_CONVERSION_COMPRESSED,
    Ucompressed => POINT_CONVERSION_UNCOMPRESSED,
    Hybrid      => POINT_CONVERSION_HYBRID);

  -----------------
  -- Subprograms --
  -----------------
  procedure Generate_Key_Pair (Key_Pair : in out Key_Pair_Type);
  procedure Derive_Public_Key (Key_Pair : in out Key_Pair_Type; Private_Key : in Byte_Array);

  procedure Get_Private_Key (Key_Pair : in Key_Pair_Type; Private_Key : in out Big_Number_Type'Class);
  procedure Get_Public_Key  (Key_Pair : in Key_Pair_Type; Public_Key  : in out Big_Number_Type'Class; Format : in Point_Format_Kind);

  function Length        (Big_Number : in Big_Number_Type) return Positive;
  function To_Byte_Array (Big_Number : in Big_Number_Type) return Byte_Array;
  
  procedure Initialize (Item : in out Key_Pair_Type;               Curve : in Elliptical_Curve_Kind);
  procedure Initialize (Item : in out Elliptical_Curve_Point_Type; Group : in EC_Group);
-------
private
-------

  -- I'm wrapping all these C allocated access types to prevent memory leaks
  type Key_Pair_Type is new Ada.Finalization.Controlled with record
    Ptr : EC_KEY := Null_Address;
  end record;
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

  type Elliptical_Curve_Point_Type is new Ada.Finalization.Controlled with record
    Ptr : EC_Point := Null_Address;
  end record;
  overriding procedure Finalize (Item : in out Elliptical_Curve_Point_Type);

  procedure Assert (Result : in Int);
  procedure Ignore (Result : in Int);

end;
