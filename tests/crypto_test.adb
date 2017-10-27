with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Bitcoin; use Bitcoin;
with Bitcoin.Crypto; use Bitcoin.Crypto;
with Bitcoin.Encoding.Base64; use Bitcoin.Encoding.Base64;
with Bitcoin.API.OpenSSL; use Bitcoin.API.OpenSSL;

procedure Crypto_Test is

  function Image (Key_Pair : in Key_Pair_Type) return String is
    Output : Unbounded_String;
  begin
    Append (Output, "-----BEGIN EC PARAMETERS-----"                                           & ASCII.CR & ASCII.LF);
    Append (Output, "EC Parameters (group) should equal BgUrgQQACg== when I get around to it" & ASCII.CR & ASCII.LF);
    Append (Output, "-----END EC PARAMETERS-----"                                             & ASCII.CR & ASCII.LF);
    Append (Output, "-----BEGIN EC PRIVATE KEY-----"                                          & ASCII.CR & ASCII.LF);
    Append (Output, To_String (Encode (Get_Private_Key (Key_Pair)))                           & ASCII.CR & ASCII.LF);
    Append (Output, "-----END EC PRIVATE KEY-----"                                            & ASCII.CR & ASCII.LF);
    Append (Output, "-----BEGIN PUBLIC KEY-----"                                              & ASCII.CR & ASCII.LF);
    Append (Output, To_String (Encode (Get_Public_Key (Key_Pair, Compressed)))                & ASCII.CR & ASCII.LF);
    Append (Output, "-----END PUBLIC KEY-----"                                                & ASCII.CR & ASCII.LF);
    return To_String (Output);
  end;

  procedure Test_Generate_Key_Pair is
    Key_Pair : Key_Pair_Type;
  begin
    Initialize (Key_Pair, secp256k1);
    Generate_Key_Pair (Key_Pair);

    Put_Line ("Generating new SECP256k1 Keypair");
    Put_Line (Image (Key_Pair));
    Put_Line ("");
  end;

  procedure Test_Derive_Public_Key is
    INITIAL_PRIVATE_KEY : Byte_Array := (
      16#16#, 16#26#, 16#07#, 16#83#, 16#e4#, 16#0b#, 16#16#, 16#73#,
      16#16#, 16#73#, 16#62#, 16#2a#, 16#c8#, 16#a5#, 16#b0#, 16#45#,
      16#fc#, 16#3e#, 16#a4#, 16#af#, 16#70#, 16#f7#, 16#27#, 16#f3#,
      16#f9#, 16#e9#, 16#2b#, 16#dd#, 16#3a#, 16#1d#, 16#dc#, 16#42#);

    Key_Pair : Key_Pair_Type;
  begin
    Initialize (Key_Pair, secp256k1);
    Derive_Public_Key (Key_Pair, INITIAL_PRIVATE_KEY);

    Put_Line ("Deriving SECP256k1 Keypair from Private Key");
    Put_Line (Image (Key_Pair));
    Put_Line ("");
  end;

begin
  Test_Generate_Key_Pair;
  Test_Derive_Public_Key;
end;
