
with Ada.Text_IO; use Ada.Text_IO;
with Bitcoin; use Bitcoin;
with Bitcoin.Crypto; use Bitcoin.Crypto;
with Bitcoin.Encoding.Base64; use Bitcoin.Encoding.Base64;
with Bitcoin.API.OpenSSL; use Bitcoin.API.OpenSSL;

procedure Crypto_Test is
  INITIAL_PRIVATE_KEY : constant Byte_Array := (
    16#16#, 16#26#, 16#07#, 16#83#, 16#e4#, 16#0b#, 16#16#, 16#73#,
    16#16#, 16#73#, 16#62#, 16#2a#, 16#c8#, 16#a5#, 16#b0#, 16#45#,
    16#fc#, 16#3e#, 16#a4#, 16#af#, 16#70#, 16#f7#, 16#27#, 16#f3#,
    16#f9#, 16#e9#, 16#2b#, 16#dd#, 16#3a#, 16#1d#, 16#dc#, 16#42#);

  Key_Pair : Key_Pair_Type;
  -- Private_Key : Big_Number_Type;
  -- Public_Key  : Big_Number_Type;
  -- Key   : EC_KEY;
  -- Group : EC_GROUP;
begin
  Initialize (Key_Pair, secp256k1);
  Derive_Public_Key (Key_Pair, INITIAL_PRIVATE_KEY);
  -- Get_Private_Key (Key_Pair, Private_Key);
  -- Get_Public_Key (Key_Pair, Public_Key, Compressed);

  Put_Line ("-----BEGIN EC PARAMETERS-----");
  Put_Line ("EC Parameters (group) should equal BgUrgQQACg== when I get around to it");
  Put_Line ("-----END EC PARAMETERS-----");
  Put_Line ("-----BEGIN EC PRIVATE KEY-----");
  -- Put_Line (To_String (Encode (To_Byte_Array (Private_Key))));
  Put_Line ("-----END EC PRIVATE KEY-----");
  Put_Line ("-----BEGIN PUBLIC KEY-----");
  -- Put_Line (To_String (Encode (To_Byte_Array (Public_Key))));
  Put_Line ("-----END PUBLIC KEY-----");
end;
