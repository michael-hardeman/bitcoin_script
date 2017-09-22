
with Ada.Text_IO; use Ada.Text_IO;
with OpenSSL; use OpenSSL;
with OpenSSL.Crypto.Thick; use OpenSSL.Crypto.Thick;

procedure OpenSSL_Create_Keypair_Test is
  INITIAL_PRIVATE_KEY : constant Byte_Array := (
    16#16#, 16#26#, 16#07#, 16#83#, 16#e4#, 16#0b#, 16#16#, 16#73#,
    16#16#, 16#73#, 16#62#, 16#2a#, 16#c8#, 16#a5#, 16#b0#, 16#45#,
    16#fc#, 16#3e#, 16#a4#, 16#af#, 16#70#, 16#f7#, 16#27#, 16#f3#,
    16#f9#, 16#e9#, 16#2b#, 16#dd#, 16#3a#, 16#1d#, 16#dc#, 16#42#);

  Key_Pair    : Key_Pair_Type;
  Private_Key : Big_Number_Type;
  Public_Key  : Big_Number_Type;
begin
  Derive_Public_Key (Key_Pair, INITIAL_PRIVATE_KEY);
  Get_Private_Key (Private_Key, Key_Pair);
  Get_Public_Key (Public_Key, Key_Pair, Compressed);

  Put_Line ("-----BEGIN EC PARAMETERS-----");
  Put_Line ("EC Parameters (group) should equal BgUrgQQACg== when I get around to it");
  Put_Line ("-----END EC PARAMETERS-----");
  Put_Line ("-----BEGIN EC PRIVATE KEY-----");
  declare Priv : Byte_Array (1 .. Length (Private_Key)) := (others => 16#00#); begin
    To_Byte_Array (Private_Key, Priv);
    Put_Line (Base_64_Encode (Priv));
  end;
  Put_Line ("-----END EC PRIVATE KEY-----");
  Put_Line ("-----BEGIN PUBLIC KEY-----");
  declare Pub : Byte_Array (1 .. Length (Public_Key)) := (others => 16#00#); begin
    To_Byte_Array (Public_Key, Pub);
    Put_Line (Base_64_Encode (Pub));
  end;
  Put_Line ("-----END PUBLIC KEY-----");
end;
