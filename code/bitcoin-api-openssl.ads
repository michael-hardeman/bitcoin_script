with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Bitcoin.API.OpenSSL is
  ---------------
  -- Constants --
  ---------------
  NID_secp112r1 : constant Int := 704;
  NID_secp112r2 : constant Int := 705;
  NID_secp128r1 : constant Int := 706;
  NID_secp128r2 : constant Int := 707;
  NID_secp160k1 : constant Int := 708;
  NID_secp160r1 : constant Int := 709;
  NID_secp160r2 : constant Int := 710;
  NID_secp192k1 : constant Int := 711;
  NID_secp224k1 : constant Int := 712;
  NID_secp224r1 : constant Int := 713;
  NID_secp256k1 : constant Int := 714;
  NID_secp384r1 : constant Int := 715;
  NID_secp521r1 : constant Int := 716;
  NID_sect113r1 : constant Int := 717;
  NID_sect113r2 : constant Int := 718;
  NID_sect131r1 : constant Int := 719;
  NID_sect131r2 : constant Int := 720;
  NID_sect163k1 : constant Int := 721;
  NID_sect163r1 : constant Int := 722;
  NID_sect163r2 : constant Int := 723;
  NID_sect193r1 : constant Int := 724;
  NID_sect193r2 : constant Int := 725;
  NID_sect233k1 : constant Int := 726;
  NID_sect233r1 : constant Int := 727;
  NID_sect239k1 : constant Int := 728;
  NID_sect283k1 : constant Int := 729;
  NID_sect283r1 : constant Int := 730;
  NID_sect409k1 : constant Int := 731;
  NID_sect409r1 : constant Int := 732;
  NID_sect571k1 : constant Int := 733;
  NID_sect571r1 : constant Int := 734;

  POINT_CONVERSION_COMPRESSED   : constant Int := 2; -- the point is encoded as z||x||y, where z is the octet 0x04
  POINT_CONVERSION_UNCOMPRESSED : constant Int := 4; -- the point is encoded as z||x||y, where the octet z specifies
  POINT_CONVERSION_HYBRID       : constant Int := 6; -- which solution of the quadratic equation y is

  BIO_FLAGS_BASE64_NO_NL : constant Int := 16#100#;
  BIO_CTRL_FLUSH         : constant Int := 11;
  BIO_C_GET_BUF_MEM_PTR  : constant Int := 115;
  BIO_CTRL_SET_CLOSE     : constant Int := 9;
  BIO_NOCLOSE            : constant Int := 16#00#;


  -----------
  -- Types --
  -----------
  subtype EC_KEY    is Address;
  subtype BIGNUM    is Address;
  subtype BN_CTX    is Address;
  subtype EC_METHOD is Address;
  subtype EC_GROUP  is Address;
  subtype EC_POINT  is Address;

  type OPENSSL_INIT_SETTINGS is record
    null;
  end record;
  type OPENSSL_INIT_SETTINGS_Access is access all OPENSSL_INIT_SETTINGS;

  --------------
  -- OPEN_SSL --
  --------------
  function OPENSSL_init_crypto (opts : in Unsigned_Long; settings : in OPENSSL_INIT_SETTINGS_Access) return Int
    with Import => True, Convention => StdCall, External_Name => "OPENSSL_init_crypto";

  function OPENSSL_init_new return OPENSSL_INIT_SETTINGS_Access
    with Import => True, Convention => StdCall, External_Name => "OPENSSL_init_new";

  function OPENSSL_INIT_set_config_appname (init : in OPENSSL_INIT_SETTINGS_Access; Name : in Chars_Ptr) return Int
    with Import => True, Convention => StdCall, External_Name => "OPENSSL_INIT_set_config_appname";

   procedure OPENSSL_INIT_free (init : in OPENSSL_INIT_SETTINGS_Access)
    with Import => True, Convention => StdCall, External_Name => "OPENSSL_INIT_free";

  --------------------------
  -- ELLIPTICAL_CURVE_KEY --
  --------------------------
  function EC_KEY_new_by_curve_name (nid : in Int) return EC_KEY
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_new_by_curve_name";

  function EC_KEY_generate_key (key : EC_KEY) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_generate_key";

  function EC_KEY_get0_group (key : in EC_KEY) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_get0_group";

  -- return 1 on success and 0 if an error occurred
  function EC_KEY_set_group (key : in EC_KEY; group : in EC_GROUP) return Int
    with Import => True, Convention => StdCall, External_Name => "EC_KEY_set_group";

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


  ----------------------------
  -- ELLIPTICAL_CURVE_GROUP --
  ----------------------------

  function EC_GROUP_dup (src : in EC_GROUP) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_dup";

  function EC_GROUP_new (meth : in EC_METHOD) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_new";

  function EC_GROUP_new_by_curve_name (nid : in Int) return EC_GROUP
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_new_by_curve_name";

  procedure EC_GROUP_free (group : in EC_GROUP)
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_free";

  procedure EC_GROUP_clear_free (group : in EC_GROUP)
    with Import => True, Convention => StdCall, External_Name => "EC_GROUP_clear_free";

  ----------------------------
  -- ELLIPTICAL_CURVE_POINT --
  ----------------------------
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

  --------------------
  -- BIGNUM_CONTEXT --
  --------------------
  function BN_CTX_new return BN_CTX
    with Import => True, Convention => StdCall, External_Name => "BN_CTX_new";

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

  procedure ERR_error_string (e : in Unsigned_Long; buf : in chars_ptr)
    with Import => True, Convention => StdCall, External_Name => "ERR_error_string";


  Assertion_Failed : exception;
end;
