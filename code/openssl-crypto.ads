with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package OpenSSL.Crypto is
  ---------------
  -- Constants --
  ---------------
  NID_secp256k1                 : constant Int := 714;

  POINT_CONVERSION_COMPRESSED   : constant Int := 2; -- the point is encoded as z||x||y, where z is the octet 0x04
  POINT_CONVERSION_UNCOMPRESSED : constant Int := 4; -- the point is encoded as z||x||y, where the octet z specifies
  POINT_CONVERSION_HYBRID       : constant Int := 6; -- which solution of the quadratic equation y is

  -----------
  -- Types --
  -----------
  subtype EC_KEY   is Address;
  subtype BIGNUM   is Address;
  subtype BN_CTX   is Address;
  subtype EC_Group is Address;
  subtype EC_Point is Address;

  ------------
  -- EC_KEY --
  ------------

  function EC_KEY_new_by_curve_name (nid : in Interfaces.C.Int) return EC_KEY
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_new_by_curve_name";

  function EC_KEY_generate_key (key : EC_KEY) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_generate_key";

  function EC_KEY_get0_group (key : in EC_KEY) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_get0_group";

  function EC_KEY_get0_private_key(key : in EC_KEY) return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_get0_private_key";

  function EC_KEY_set_private_key(key : in EC_KEY; prv : in BIGNUM) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_set_private_key";

  function EC_KEY_get0_public_key(key : in EC_KEY) return EC_POINT
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_get0_public_key";

  function EC_KEY_set_public_key (key : in EC_KEY; pub : in EC_POINT) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_set_public_key";

  procedure EC_KEY_free (a : in EC_KEY)
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_free";

  -- RETURNS LENGTH OF output param
  -- Output must be the address of a Interfaces.C.Chars_Ptr
  function i2d_ECPKParameters(x : EC_GROUP; output : Address) return Int
    with Import => True, Convention => StdCall, External_Name => "i2d_ECPKParameters";

  -- RETURNS LENGTH OF output param
  -- Output must be the address of a Interfaces.C.Chars_Ptr
  function i2d_ECPrivateKey(x : EC_KEY; output : Address) return Int
    with Import => True, Convention => StdCall, External_Name => "i2d_ECPrivateKey";

  --------------
  -- EC_POINT --
  --------------
  function EC_POINT_new (group : in EC_GROUP) return EC_POINT
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_new";

  function EC_POINT_mul (
    group : in EC_GROUP;
    r     : in EC_POINT;
    n     : in BIGNUM;
    q     : in EC_POINT;
    m     : in BIGNUM;
    ctx   : in BN_CTX)
    return Int
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_mul";

  procedure EC_POINT_free (point : in EC_POINT)
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_free";

  function EC_POINT_point2bn(group : EC_GROUP; p : EC_POINT; form : Int; bn : BIGNUM; ctx : BN_CTX) return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_point2bn";

  ------------
  -- BN_CTX --
  ------------
  function BN_CTX_new return BN_CTX
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_new";

  procedure BN_CTX_start(ctx : in BN_CTX)
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_start";

  procedure BN_CTX_end (ctx : in BN_CTX)
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_end";

  procedure BN_CTX_free (ctx : in BN_CTX)
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_free";

  --------
  -- BN --
  --------
  function BN_new return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "BN_new";

  function BN_num_bits (a : in BIGNUM) return Int
    with Import => True, Convention => StdCall, External_Name => "BN_num_bits";

  function BN_num_bytes (a : in BIGNUM) return Int is ((BN_num_bits (a)+7)/8);

  -- RETURNS LENGTH OF to param
  function BN_bn2bin (a : in BIGNUM; to : in Address) return Int
    with Import => True, Convention => StdCall, External_Name => "BN_bn2bin";

  function BN_bin2bn (s : in Address; len : in Int; ret : in BIGNUM) return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "BN_bin2bn";

  procedure BN_clear_free (a : in BIGNUM)
    with Import => True, Convention => StdCall, External_Name => "BN_clear_free";

  function ERR_get_error return Unsigned_Long
    with Import => True, Convention => StdCall, External_Name => "ERR_get_error";

  procedure ERR_error_string(e : in Unsigned_Long; buf : in chars_ptr)
    with Import => True, Convention => StdCall, External_Name => "ERR_error_string";

  Assertion_Failed : exception;
end;
