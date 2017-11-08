with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Bitcoin;                 use Bitcoin;
with Bitcoin.Crypto;          use Bitcoin.Crypto;
with Bitcoin.API.OpenSSL;     use Bitcoin.API.OpenSSL;
with Bitcoin.Encoding.Base64; use Bitcoin.Encoding.Base64;

procedure Crypto_Test is

  -----------
  -- Image --
  -----------
  function Image (Key_Pair : in Key_Pair_Type) return String is
    Output : Unbounded_String;
  begin
    Append (Output, "-----BEGIN EC PRIVATE KEY-----"                           & ASCII.CR & ASCII.LF);
    Append (Output, To_String (Encode (Get_Private_Key (Key_Pair)))            & ASCII.CR & ASCII.LF);
    Append (Output, "-----END EC PRIVATE KEY-----"                             & ASCII.CR & ASCII.LF);
    Append (Output, "-----BEGIN PUBLIC KEY-----"                               & ASCII.CR & ASCII.LF);
    Append (Output, To_String (Encode (Get_Public_Key (Key_Pair, Compressed))) & ASCII.CR & ASCII.LF);
    Append (Output, "-----END PUBLIC KEY-----"                                 & ASCII.CR & ASCII.LF);
    return To_String (Output);
  end;

  ----------------------------
  -- Test_Generate_Key_Pair --
  ----------------------------
  procedure Test_Generate_Key_Pair is
    Key_Pair : Key_Pair_Type;
  begin
    Generate (Key_Pair, secp256k1);

    Put_Line ("Generating new Keypair");
    Put_Line (Image (Key_Pair));
    Put_Line ("");
  end;

  ----------------------------
  -- Test_Derive_Public_Key --
  ----------------------------
  procedure Test_Derive_Public_Key is
    PRIVATE_KEY : Byte_Array := (
      16#16#, 16#26#, 16#07#, 16#83#, 16#e4#, 16#0b#, 16#16#, 16#73#,
      16#16#, 16#73#, 16#62#, 16#2a#, 16#c8#, 16#a5#, 16#b0#, 16#45#,
      16#fc#, 16#3e#, 16#a4#, 16#af#, 16#70#, 16#f7#, 16#27#, 16#f3#,
      16#f9#, 16#e9#, 16#2b#, 16#dd#, 16#3a#, 16#1d#, 16#dc#, 16#42#);

    Key_Pair : Key_Pair_Type;
  begin
    From_Private_Key (Key_Pair, secp256k1, PRIVATE_KEY);

    Put_Line ("Deriving from known Private Key");
    Put_Line (Image (Key_Pair));
    Put_Line ("");
  end;

  --------------------------
  -- Test_Sign_And_Verify --
  --------------------------
  procedure Test_Sign_And_Verify is
    PRIVATE_KEY : Byte_Array := (
      16#16#, 16#26#, 16#07#, 16#83#, 16#e4#, 16#0b#, 16#16#, 16#73#,
      16#16#, 16#73#, 16#62#, 16#2a#, 16#c8#, 16#a5#, 16#b0#, 16#45#,
      16#fc#, 16#3e#, 16#a4#, 16#af#, 16#70#, 16#f7#, 16#27#, 16#f3#,
      16#f9#, 16#e9#, 16#2b#, 16#dd#, 16#3a#, 16#1d#, 16#dc#, 16#42#);

    MESSAGE : Byte_Array := To_Byte_Array ("I approve this message.");

    Key_Pair : Key_Pair_Type;
  begin
    From_Private_Key (Key_Pair, secp256k1, PRIVATE_KEY);

    Put_Line ("Sign and verify");
    declare Signature : Byte_Array := Sign (Key_Pair, MESSAGE); begin
      Put_Line ("Message Bytes     : " & Image (MESSAGE));
      Put_Line ("Message Base64    : " & To_String (Encode (MESSAGE)));
      Put_Line ("Signature Bytes   : " & Image (Signature));
      Put_Line ("Signature Base64  : " & To_String (Encode (Signature)));
      Put_Line ("Verfication Status: " & Boolean'Image (Verify (Key_Pair, Signature, Message)));
    end;
    Put_Line ("");
  end;

begin
  Test_Generate_Key_Pair;
  Test_Derive_Public_Key;
  Test_Sign_And_Verify;
end;
