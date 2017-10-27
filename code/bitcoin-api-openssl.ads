with System;               use System;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Bitcoin.API.OpenSSL is

  ---------------
  -- Constants --
  ---------------
  NID_secp112r1 : constant Unsigned := 704;
  NID_secp112r2 : constant Unsigned := 705;
  NID_secp128r1 : constant Unsigned := 706;
  NID_secp128r2 : constant Unsigned := 707;
  NID_secp160k1 : constant Unsigned := 708;
  NID_secp160r1 : constant Unsigned := 709;
  NID_secp160r2 : constant Unsigned := 710;
  NID_secp192k1 : constant Unsigned := 711;
  NID_secp224k1 : constant Unsigned := 712;
  NID_secp224r1 : constant Unsigned := 713;
  NID_secp256k1 : constant Unsigned := 714;
  NID_secp384r1 : constant Unsigned := 715;
  NID_secp521r1 : constant Unsigned := 716;
  NID_sect113r1 : constant Unsigned := 717;
  NID_sect113r2 : constant Unsigned := 718;
  NID_sect131r1 : constant Unsigned := 719;
  NID_sect131r2 : constant Unsigned := 720;
  NID_sect163k1 : constant Unsigned := 721;
  NID_sect163r1 : constant Unsigned := 722;
  NID_sect163r2 : constant Unsigned := 723;
  NID_sect193r1 : constant Unsigned := 724;
  NID_sect193r2 : constant Unsigned := 725;
  NID_sect233k1 : constant Unsigned := 726;
  NID_sect233r1 : constant Unsigned := 727;
  NID_sect239k1 : constant Unsigned := 728;
  NID_sect283k1 : constant Unsigned := 729;
  NID_sect283r1 : constant Unsigned := 730;
  NID_sect409k1 : constant Unsigned := 731;
  NID_sect409r1 : constant Unsigned := 732;
  NID_sect571k1 : constant Unsigned := 733;
  NID_sect571r1 : constant Unsigned := 734;

  -- encoded as z||x, where the octet z specifies which solution of the quadratic equation for y
  POINT_CONVERSION_COMPRESSED   : constant Unsigned := 2;
  -- encoded as z||x||y, where z is the octet 0x04
  POINT_CONVERSION_UNCOMPRESSED : constant Unsigned := 4;
  -- encoded as z||x||y, where the octet z specifies which solution of the quadratic equation for y
  POINT_CONVERSION_HYBRID       : constant Unsigned := 6;

  EC_EXPLICIT_CURVE : constant Unsigned := 0;
  EC_NAMED_CURVE    : constant Unsigned := 1;

  -----------
  -- Types --
  -----------
  -- I tried to import the internal structure of these, but it caused lots of
  -- issues. The C types are designed to be opaque pointers so they should be
  -- the same here as well.
  subtype EC_POINT     is Address;
  subtype EC_METHOD    is Address;
  subtype EC_GROUP     is Address;
  subtype EC_KEY       is Address;
  subtype BIGNUM       is Address;
  subtype BN_CTX       is Address;
  subtype ECPARAMETERS is Address;

  --------------------------
  -- ELLIPTICAL_CURVE_KEY --
  --------------------------
  function EC_KEY_new_by_curve_name (nid : in Unsigned) return EC_KEY
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_new_by_curve_name";

  procedure EC_KEY_set_asn1_flag (eckey : in EC_KEY; asn1_flag : in Unsigned)
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_set_asn1_flag";

  function EC_KEY_generate_key (key : in EC_KEY) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_generate_key";

  function EC_KEY_get0_group (key : in EC_KEY) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_get0_group";

  function EC_KEY_set_group (key : in EC_KEY; group : in EC_GROUP) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_set_group";

  function EC_KEY_get0_private_key (key : in EC_KEY) return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_get0_private_key";

  function EC_KEY_set_private_key (key : in EC_KEY; prv : in BIGNUM) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_set_private_key";

  function EC_KEY_get0_public_key (key : in EC_KEY) return EC_POINT
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_get0_public_key";

  function EC_KEY_set_public_key (key : in EC_KEY; pub : in EC_POINT) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_set_public_key";

  procedure EC_KEY_free (a : in EC_KEY)
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_free";

  ----------------------------
  -- ELLIPTICAL_CURVE_GROUP --
  ----------------------------
  function EC_GROUP_dup (src : in EC_GROUP) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_dup";

  function EC_GROUP_new (meth : in EC_METHOD) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_new";

  function EC_GROUP_new_by_curve_name (nid : in Unsigned) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_new_by_curve_name";

  procedure EC_GROUP_free (group : in EC_GROUP)
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_free";

  procedure EC_GROUP_clear_free (group : in EC_GROUP)
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_clear_free";

  -- ECPARAMETERS *EC_GROUP_get_ecparameters(const EC_GROUP *group, ECPARAMETERS *params)

  ----------------------------
  -- ELLIPTICAL_CURVE_POINT --
  ----------------------------
  function EC_POINT_new (group : in EC_GROUP) return EC_POINT
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_new";

  function EC_POINT_mul (
    group : in     EC_GROUP;
    r     : in EC_POINT;
    n     : in BIGNUM;
    q     : in EC_POINT;
    m     : in BIGNUM;
    ctx   : in BN_CTX)
    return Int
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_mul";

  function EC_POINT_point2bn(
    group : in EC_GROUP;
    p     : in EC_POINT;
    form  : in Unsigned;
    bn    : in BIGNUM;
    ctx   : in BN_CTX)
    return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_point2bn";

  procedure EC_POINT_clear_free (point : in EC_POINT)
    with Import => True, Convention => StdCall, External_Name => "EC_POINT_clear_free";

  --------------------
  -- BIGNUM_CONTEXT --
  --------------------
  function BN_CTX_secure_new return BN_CTX
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_secure_new";

  procedure BN_CTX_start(ctx : in BN_CTX)
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_start";

  procedure BN_CTX_end (ctx : in BN_CTX)
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_end";

  procedure BN_CTX_free (ctx : in BN_CTX)
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_free";

  ------------
  -- BIGNUM --
  ------------
  function BN_new return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "BN_new";

  -- returns Length of a in bits
  function BN_num_bits (a : in BIGNUM) return Int
    with Import => True, Convention => StdCall, External_Name => "BN_num_bits";

  -- returns Length of a in bytes
  function BN_num_bytes (a : in BIGNUM) return Int is ((BN_num_bits (a)+7)/8);

  -- returns the number of bytes written in the to parameter
  function BN_bn2bin (a : in BIGNUM; to : in Byte_Access) return Int
    with Import => True, Convention => StdCall, External_Name => "BN_bn2bin";

  function BN_bin2bn (s : in Byte_Access; len : in Int; ret : in BIGNUM) return BIGNUM
    with Import => True, Convention => StdCall, External_Name => "BN_bin2bn";

  procedure BN_clear_free (a : in BIGNUM)
    with Import => True, Convention => StdCall, External_Name => "BN_clear_free";

  -----------
  -- Error --
  -----------
  function ERR_get_error return Unsigned_Long
    with Import => True, Convention => StdCall, External_Name => "ERR_get_error";

  procedure ERR_error_string (e : in Unsigned_Long; buf : in chars_ptr)
    with Import => True, Convention => StdCall, External_Name => "ERR_error_string";

end;
