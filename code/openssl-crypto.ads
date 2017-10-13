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

  BIO_FLAGS_BASE64_NO_NL        : constant Int := 16#100#;
  BIO_CTRL_FLUSH                : constant Int := 11;
  BIO_C_GET_BUF_MEM_PTR         : constant Int := 115;
  BIO_CTRL_SET_CLOSE            : constant Int := 9;
  BIO_NOCLOSE                   : constant Int := 16#00#;


  -----------
  -- Types --
  -----------
  subtype EC_KEY     is Address;
  subtype BIGNUM     is Address;
  subtype BN_CTX     is Address;
  subtype EC_Group   is Address;
  subtype EC_Point   is Address;
  subtype BIO        is Address;
  subtype BIO_METHOD is Address;

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

  -------------------------
  -- BINARY_INPUT_OUTPUT --
  -------------------------
  function BIO_new (kind : in BIO_METHOD) return BIO
    with Import => True, Convention => StdCall, External_Name => "BIO_new";

  procedure BIO_free_all (a : in BIO)
    with Import => True, Convention => StdCall, External_Name => "BIO_free_all";

  function BIO_f_base64 return BIO_METHOD
    with Import => True, Convention => StdCall, External_Name => "BIO_f_base64";

  function BIO_s_mem return BIO_METHOD
    with Import => True, Convention => StdCall, External_Name => "BIO_s_mem";

  function BIO_push (b : in BIO; append : in BIO) return BIO
    with Import => True, Convention => StdCall, External_Name => "BIO_push";

  function BIO_pop (b : in BIO) return BIO
    with Import => True, Convention => StdCall, External_Name => "BIO_pop";

  procedure BIO_set_flags (b : in BIO; flags : in Int)
    with Import => True, Convention => StdCall, External_Name => "BIO_set_flags";

  function BIO_write (b : in BIO; data : in Address; len : in Int) return Int
    with Import => True, Convention => StdCall, External_Name => "BIO_write";

  function BIO_ctrl (b : in BIO; cmd : in Int; larg : in Long; parg : in Address) return Long
    with Import => True, Convention => StdCall, External_Name => "BIO_ctrl";

  function BIO_Flush (b : in BIO) return Int is (Int (BIO_ctrl (b, BIO_CTRL_FLUSH, 0, Null_Address)));

  function BIO_get_mem_ptr (b : in BIO; pp : Address) return Int is (Int (BIO_ctrl (b, BIO_C_GET_BUF_MEM_PTR, 0, pp)));

  function BIO_set_close (b : in BIO; c : in Int) return Int is (Int (BIO_ctrl (b, BIO_CTRL_SET_CLOSE, Long (c), Null_Address)));

  Assertion_Failed : exception;
end;
