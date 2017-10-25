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

  POINT_CONVERSION_COMPRESSED   : constant Unsigned := 2; -- the point is encoded as z||x||y, where z is the octet 0x04
  POINT_CONVERSION_UNCOMPRESSED : constant Unsigned := 4; -- the point is encoded as z||x||y, where the octet z specifies
  POINT_CONVERSION_HYBRID       : constant Unsigned := 6; -- which solution of the quadratic equation y is

  BIO_FLAGS_BASE64_NO_NL : constant Int := 16#100#;
  BIO_CTRL_FLUSH         : constant Int := 11;
  BIO_C_GET_BUF_MEM_PTR  : constant Int := 115;
  BIO_CTRL_SET_CLOSE     : constant Int := 9;
  BIO_NOCLOSE            : constant Int := 16#00#;

  -----------
  -- Types --
  -----------
  subtype BN_CTX    is Address;
  subtype EC_POINT  is Address;

  type EC_METHOD;
  type ENGINE
  type EC_GROUP;
  type EC_KEY;
  type BIGNUM;
  type stack_st;

  type EC_METHOD       is access all EC_METHOD;
  type ENGINE_Access   is access all ENGINE;
  type EC_KEY_Access   is access all EC_KEY;
  type BIGNUM_Access   is access all BIGNUM;
  type stack_st_Access is access all stack_st;

  type group_init_function               is access function  (Group : EC_GROUP_Access) return Int;
  type group_finish_procedure            is access procedure (Group : EC_GROUP_Access);
  type group_clear_finish_procedure      is access procedure (Group : EC_GROUP_Access);
  type group_copy_function               is access function  (Group : EC_GROUP_Access; Target : EC_GROUP_Access) return Int;
  type group_set_curve_function          is access function  (Group : EC_GROUP_Access; P : BIGNUM_Access; A : BIGNUM_Access; B : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access);
  type group_get_curve_function          is access function  (Group : EC_GROUP_Access; P : BIGNUM_Access; A : BIGNUM_Access; B : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access);
  type group_get_degree_function         is access function  (Group : EC_GROUP_Access) return Int;
  type group_order_bits_function         is access function  (Group : EC_GROUP_Access) return Int;
  type group_check_discriminant_function is access function  (Group : EC_GROUP_Access; Context : BIGNUM_CONTEXT_Access) return Int;

  type point_init_function          is access function  (Point : EC_POINT_Access) return Int;
  type point_finish_procedure       is access procedure (Point : EC_POINT_Access);
  type point_clear_finish_procedure is access procedure (Point : EC_POINT_Access);
  type point_copy                   is access function  (Point : EC_POINT_Access; Target : EC_POINT_Access) return Int;

  type point_set_to_infinity_function                 is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access) return Int;
  type point_set_Jprojective_coordinates_GFp_function is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; X : BIGNUM_Access; Y : BIGNUM_Access; Z : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type point_get_Jprojective_coordinates_GFp_function is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; X : BIGNUM_Access; Y : BIGNUM_Access; Z : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type point_set_affine_coordinates_function          is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; X : BIGNUM_Access; Y : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type point_get_affine_coordinates_function          is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; X : BIGNUM_Access; Y : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type point_set_compressed_coordinates_function      is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; X : BIGNUM_Access; Y_Bit : Int; Context : BIGNUM_CONTEXT_Access) return Int;

  type point2oct_function is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; Form : Unsigned; Buf : Chars_Ptr; Len : Size_T; Context : BIGNUM_CONTEXT_Access) return Size_T;
  type oct2point_function is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; Buf : Chars_Ptr; Len : Size_T; Context : BIGNUM_CONTEXT_Access) return Int;

  type add_function    is access function (Group : EC_GROUP_Access; R : EC_POINT_Access; A : EC_POINT_Access; B : EC_POINT_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type dbl_function    is access function (Group : EC_GROUP_Access; R : EC_POINT_Access; A : EC_POINT_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type invert_function is access function (Group : EC_GROUP_Access; A : EC_POINT_Access; Context : BIGNUM_CONTEXT_Access) return Int;

  type is_at_infinity_function is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access);
  type is_on_curve_function    is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type point_cmp_function      is access function (Group : EC_GROUP_Access; A : EC_POINT_Access; B : EC_POINT_Access; Context : BIGNUM_CONTEXT_Access) return Int;

  type make_affine_function        is access function (Group : EC_GROUP_Access; Point : EC_POINT_Access; BIGNUM_CONTEXT_Access) return Int;
  type points_make_affine_function is access function (Group : EC_GROUP_Access; Num : Size_T; Points : EC_Point_Access; Context : BIGNUM_CONTEXT_Access) return Int;

  type mul_function             is access function (Group : EC_GROUP_Access; R : EC_POINT_Access; Scalar : BIGNUM_Access; Num : Size_T; Points : EC_POINT_Access; scalars : BIGNUM_CONTEXT_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type precompute_mult_function is access function (Group : EC_GROUP_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type have_precompute_mult     is access function (Group : EC_GROUP_Access) return Int;

  type field_mul_function is access function (Group : EC_GROUP_Access; R : BIGNUM_Access; A : BIGNUM_Access; B : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type field_sqr_function is access function (Group : EC_GROUP_Access; R : BIGNUM_Access; A : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type field_div_function is access function (Group : EC_GROUP_Access; R : BIGNUM_Access; A : BIGNUM_Access; B : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int; 

  type field_encode_function is access function (Group : EC_GROUP_Access; R : BIGNUM_Access; A : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;

  type field_decode_function     is access function (Group : EC_GROUP_Access; R : BIGNUM_Access; A : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;
  type field_set_to_one_function is access function (Group : EC_GROUP_Access; R : BIGNUM_Access; Context : BIGNUM_CONTEXT_Access) return Int;

  type priv2oct_function    is access function  (Eckey : EC_KEY_Access; Buf : Chars_Ptr; Len : Size_T) return Size_T;
  type oct2priv_function    is access function  (Eckey : EC_KEY_Access; Buf : Chars_Ptr; Len : Size_T) return Int;
  type set_private_function is access function  (Eckey : EC_KEY_Access; Priv_Key : BIGNUM_Access) return Int;
  type keygen_function      is access function  (Eckey : EC_KEY_Access) return Int;
  type keycheck_function    is access function  (Eckey : EC_KEY_Access) return Int;
  type keygenpub_function   is access function  (Eckey : EC_KEY_Access) return Int;
  type keycopy_function     is access function  (Dst : EC_KEY_Access; Src : EC_KEY_Access) return Int;
  type keyfinish_procedure  is access procedure (Eckey : EC_KEY_Access) return Int;

  type ecdh_compute_key_function is access function (Pout : Chars_Ptr_Ptr; Pout_Len : Size_T; Pub_Key : EC_POINT_Access; ECDSA : EC_KEY_Access) return Int;

  type EC_METHOD is record
    -- Various method flags
    flags : Int; -- int
    -- used by EC_METHOD_get_field_type:
    field_type : Int; -- int a NID
    -- used by EC_GROUP_new, EC_GROUP_free, EC_GROUP_clear_free, EC_GROUP_copy: 
    group_init         : group_init_function;          -- int (*group_init) (EC_GROUP *);
    group_finish       : group_finish_procedure;       -- void (*group_finish) (EC_GROUP *);
    group_clear_finish : group_clear_finish_procedure; -- void (*group_clear_finish) (EC_GROUP *);
    group_copy         : group_copy_function;          -- int (*group_copy) (EC_GROUP *, const EC_GROUP *);
    -- used by EC_GROUP_set_curve_GFp, EC_GROUP_get_curve_GFp,
    -- EC_GROUP_set_curve_GF2m, and EC_GROUP_get_curve_GF2m:
    group_set_curve : group_set_curve_function; -- int (*group_set_curve) (EC_GROUP *, const BIGNUM *p, const BIGNUM *a, const BIGNUM *b, BN_CTX *);
    group_get_curve : group_get_curve_function; -- int (*group_get_curve) (const EC_GROUP *, BIGNUM *p, BIGNUM *a, BIGNUM *b, BN_CTX *);
    -- used by EC_GROUP_get_degree:
    group_get_degree : group_get_degree_function; -- int (*group_get_degree) (const EC_GROUP *);
    group_order_bits : group_order_bits_function; -- int (*group_order_bits) (const EC_GROUP *);
    -- used by EC_GROUP_check:
    group_check_discriminant : ; -- int (*group_check_discriminant) (const EC_GROUP *, BN_CTX *);
    -- used by EC_POINT_new, EC_POINT_free, EC_POINT_clear_free, EC_POINT_copy:
    point_init         : point_init_function;          -- int (*point_init) (EC_POINT *);
    point_finish       : point_finish_procedure;       -- void (*point_finish) (EC_POINT *);
    point_clear_finish : point_clear_finish_procedure; -- void (*point_clear_finish) (EC_POINT *);
    point_copy         : point_copy_function;          -- int (*point_copy) (EC_POINT *, const EC_POINT *);
    -- used by EC_POINT_set_to_infinity,
    -- EC_POINT_set_Jprojective_coordinates_GFp,
    -- EC_POINT_get_Jprojective_coordinates_GFp,
    -- EC_POINT_set_affine_coordinates_GFp,     ..._GF2m,
    -- EC_POINT_get_affine_coordinates_GFp,     ..._GF2m,
    -- EC_POINT_set_compressed_coordinates_GFp, ..._GF2m:
    point_set_to_infinity                 : point_set_to_infinity_function;                 -- int (*point_set_to_infinity) (const EC_GROUP *, EC_POINT *);
    point_set_Jprojective_coordinates_GFp : point_set_Jprojective_coordinates_GFp_function; -- int (*point_set_Jprojective_coordinates_GFp) (const EC_GROUP *, EC_POINT *, const BIGNUM *x, const BIGNUM *y, const BIGNUM *z, BN_CTX *);
    point_get_Jprojective_coordinates_GFp : point_get_Jprojective_coordinates_GFp_function; -- int (*point_get_Jprojective_coordinates_GFp) (const EC_GROUP *, const EC_POINT *, BIGNUM *x, BIGNUM *y, BIGNUM *z, BN_CTX *);
    point_set_affine_coordinates          : point_set_affine_coordinates_function;          -- int (*point_set_affine_coordinates) (const EC_GROUP *, EC_POINT *, const BIGNUM *x, const BIGNUM *y, BN_CTX *);
    point_get_affine_coordinates          : point_get_affine_coordinates_function;          -- int (*point_get_affine_coordinates) (const EC_GROUP *, const EC_POINT *, BIGNUM *x, BIGNUM *y, BN_CTX *);
    point_set_compressed_coordinates      : point_set_compressed_coordinates_function;      -- int (*point_set_compressed_coordinates) (const EC_GROUP *, EC_POINT *, const BIGNUM *x, int y_bit, BN_CTX *);
    -- used by EC_POINT_point2oct, EC_POINT_oct2point:
    point2oct : point2oct_function; -- size_t (*point2oct) (const EC_GROUP *, const EC_POINT *, point_conversion_form_t form, unsigned char *buf, size_t len, BN_CTX *);
    oct2point : oct2point_function; -- int (*oct2point) (const EC_GROUP *, EC_POINT *, const unsigned char *buf, size_t len, BN_CTX *);
    -- used by EC_POINT_add, EC_POINT_dbl, ECP_POINT_invert:
    add    : add_function;    -- int (*add) (const EC_GROUP *, EC_POINT *r, const EC_POINT *a, const EC_POINT *b, BN_CTX *);
    dbl    : dbl_function;    -- int (*dbl) (const EC_GROUP *, EC_POINT *r, const EC_POINT *a, BN_CTX *);
    invert : invert_function; -- int (*invert) (const EC_GROUP *, EC_POINT *, BN_CTX *);
    -- used by EC_POINT_is_at_infinity, EC_POINT_is_on_curve, EC_POINT_cmp:
    is_at_infinity : is_at_infinity_function; -- int (*is_at_infinity) (const EC_GROUP *, const EC_POINT *);
    is_on_curve    : is_on_curve_function;    -- int (*is_on_curve) (const EC_GROUP *, const EC_POINT *, BN_CTX *);
    point_cmp      : point_cmp_function;      -- int (*point_cmp) (const EC_GROUP *, const EC_POINT *a, const EC_POINT *b, BN_CTX *);
    -- used by EC_POINT_make_affine, EC_POINTs_make_affine:
    make_affine        : make_affine_function; -- int (*make_affine) (const EC_GROUP *, EC_POINT *, BN_CTX *);
    points_make_affine : points_make_affine_function; -- int (*points_make_affine) (const EC_GROUP *, size_t num, EC_POINT *[], BN_CTX *);
    -- used by EC_POINTs_mul, EC_POINT_mul, EC_POINT_precompute_mult, EC_POINT_have_precompute_mult (default implementations are used if  'mul' pointer is 0):
    mul                  : mul_function;                  -- int (*mul) (const EC_GROUP *group, EC_POINT *r, const BIGNUM *scalar, size_t num, const EC_POINT *points[], const BIGNUM *scalars[], BN_CTX *);
    precompute_mult      : precompute_mult_function;      -- int (*precompute_mult) (EC_GROUP *group, BN_CTX *);
    have_precompute_mult : have_precompute_mult_function; -- int (*have_precompute_mult) (const EC_GROUP *group);
    -- internal functions
    --
    -- 'field_mul', 'field_sqr', and 'field_div' can be used by 'add' and
    -- 'dbl' so that the same implementations of point operations can be used
    -- with different optimized implementations of expensive field
    -- operations:
    field_mul : field_mul_function; -- int (*field_mul) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a, const BIGNUM *b, BN_CTX *);
    field_sqr : field_sqr_function; -- int (*field_sqr) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a, BN_CTX *);
    field_div : field_div_function; -- int (*field_div) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a, const BIGNUM *b, BN_CTX *);
    -- e.g. to Montgomery
    field_encode : field_encode_function; -- int (*field_encode) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a, BN_CTX *);
    -- e.g. from Montgomery
    field_decode     : field_decode_function;     -- int (*field_decode) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a, BN_CTX *);
    field_set_to_one : field_set_to_one_function; -- int (*field_set_to_one) (const EC_GROUP *, BIGNUM *r, BN_CTX *);
    -- private key operations
    priv2oct    : priv2oct_function;    -- size_t (*priv2oct)(const EC_KEY *eckey, unsigned char *buf, size_t len);
    oct2priv    : oct2priv_function;    -- int (*oct2priv)(EC_KEY *eckey, const unsigned char *buf, size_t len);
    set_private : set_private_function; -- int (*set_private)(EC_KEY *eckey, const BIGNUM *priv_key);
    keygen      : keygen_function;      -- int (*keygen)(EC_KEY *eckey);
    keycheck    : keycheck_function;    -- int (*keycheck)(const EC_KEY *eckey);
    keygenpub   : keygenpub_function;   -- int (*keygenpub)(EC_KEY *eckey);
    keycopy     : keycopy_function;     -- int (*keycopy)(EC_KEY *dst, const EC_KEY *src);
    keyfinish   : keyfinish_procedure;  -- void (*keyfinish)(EC_KEY *eckey);
    -- custom ECDH operation
    ecdh_compute_key : ecdh_compute_key_function; -- int (*ecdh_compute_key)(unsigned char **pout, size_t *poutlen, const EC_POINT *pub_key, const EC_KEY *ecdh);
  end record;

  type ENGINE is record
    id   : Chars_Ptr;     -- const char *
    name : Chars_Ptr;     -- const char *
    -- Used to maintain the linked-list of engines.
    prev : ENGINE_Access; -- struct engine_st *
    next : ENGINE_Access; -- struct engine_st *
  end record;

  type EC_GROUP is record
    struct ec_group_st {
    const EC_METHOD *meth;
    EC_POINT *generator;        /* optional */
    BIGNUM *order, *cofactor;
    int curve_name;             /* optional NID for named curve */
    int asn1_flag;              /* flag to control the asn1 encoding */
    point_conversion_form_t asn1_form;
    unsigned char *seed;        /* optional seed for parameters (appears in
                                 * ASN1) */
    size_t seed_len;
    /*
     * The following members are handled by the method functions, even if
     * they appear generic
     */
    /*
     * Field specification. For curves over GF(p), this is the modulus; for
     * curves over GF(2^m), this is the irreducible polynomial defining the
     * field.
     */
    BIGNUM *field;
    /*
     * Field specification for curves over GF(2^m). The irreducible f(t) is
     * then of the form: t^poly[0] + t^poly[1] + ... + t^poly[k] where m =
     * poly[0] > poly[1] > ... > poly[k] = 0. The array is terminated with
     * poly[k+1]=-1. All elliptic curve irreducibles have at most 5 non-zero
     * terms.
     */
    int poly[6];
    /*
     * Curve coefficients. (Here the assumption is that BIGNUMs can be used
     * or abused for all kinds of fields, not just GF(p).) For characteristic
     * > 3, the curve is defined by a Weierstrass equation of the form y^2 =
     * x^3 + a*x + b. For characteristic 2, the curve is defined by an
     * equation of the form y^2 + x*y = x^3 + a*x^2 + b.
     */
    BIGNUM *a, *b;
    /* enable optimized point arithmetics for special case */
    int a_is_minus3;
    /* method-specific (e.g., Montgomery structure) */
    void *field_data1;
    /* method-specific */
    void *field_data2;
    /* method-specific */
    int (*field_mod_func) (BIGNUM *, const BIGNUM *, const BIGNUM *,
                           BN_CTX *);
    /* data for ECDSA inverse */
    BN_MONT_CTX *mont_data;

    /*
     * Precomputed values for speed. The PCT_xxx names match the
     * pre_comp.xxx union names; see the SETPRECOMP and HAVEPRECOMP
     * macros, below.
     */
    enum {
        PCT_none,
        PCT_nistp224, PCT_nistp256, PCT_nistp521, PCT_nistz256,
        PCT_ec
    } pre_comp_type;
    union {
        NISTP224_PRE_COMP *nistp224;
        NISTP256_PRE_COMP *nistp256;
        NISTP521_PRE_COMP *nistp521;
        NISTZ256_PRE_COMP *nistz256;
        EC_PRE_COMP *ec;
    } pre_comp;
  end record;
  
  type BIGNUM is record
    d     : Address; -- BN_ULONG Pointer to an array of 'BN_BITS2' bit chunks.
    top   : Int;     -- int      Index of last used d +1. The next are internal book keeping for bn_expand.
    dmax  : Int;     -- int      Size of the d array.
    neg   : Int;     -- int      one if the number is negative.
    flags : Int;     -- int
  end record;

  -- int (*OPENSSL_sk_compfunc)(const void *, const void *);
  type OPENSSL_sk_compfunc is access function (A : Address; B : Address) return Int;

  type stack_st is record
    num       : Int;                 -- Int
    data      : Address;             -- const void **
    sorted    : Int;                 -- int
    num_alloc : Int;                 -- int
    comp      : OPENSSL_sk_compfunc; -- OPENSSL_sk_compfunc
  end record;
  
  type crypto_ex_data_st is record
    sk : stack_st_access; -- STACK_OF(void) *sk;
  end record;

  type EC_KEY is record
    meth       : EC_METHOD_Access;  -- const EC_KEY_METHOD *
    engine     : ENGINE_Access;     -- ENGINE *
    version    : Int;               -- Int
    group      : EC_GROUP_Access;   -- EC_GROUP *
    pub_key    : EC_POINT_Access;   -- EC_POINT *
    priv_key   : BIGNUM_Access;     -- BIGNUM *
    enc_flag   : Unsigned;          -- unsigned int
    conv_form  : Unsigned;          -- point_conversion_form_t
    references : Int;               -- CRYPTO_REF_COUNT
    flags      : Int;               -- int 
    ex_data    : crypto_ex_data_st; -- CRYPTO_EX_DATA
    lock       : Address;           -- CRYPTO_RWLOCK *
  end record;


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
