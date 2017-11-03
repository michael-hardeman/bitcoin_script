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

  -----------
  -- Image --
  -----------
  function Image (Items : Byte_Array) return String is
    Output : Unbounded_String;
  begin
    Append (Output, "(");
    for I in Items'First .. Items'Last - 1 loop
      Append (Output, Byte'Image (Items (I))); Append (Output, ",");
    end loop;
    Append (Output, Byte'Image (Items (Items'Last))); Append (Output, " )");
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

    MESSAGE : Byte_Array := (
      16#97#, 16#f6#, 16#35#, 16#93#, 16#af#, 16#3c#, 16#00#, 16#55#,
      16#4d#, 16#ef#, 16#10#, 16#dc#, 16#f8#, 16#1d#, 16#b5#, 16#95#,
      16#00#, 16#60#, 16#a2#, 16#ba#, 16#4d#, 16#4e#, 16#90#, 16#94#,
      16#7f#, 16#cf#, 16#38#, 16#a0#, 16#44#, 16#3c#, 16#af#, 16#89#,
      16#c4#, 16#7e#, 16#9a#, 16#fb#, 16#97#, 16#17#, 16#db#, 16#a1#,
      16#1c#, 16#2a#, 16#fb#, 16#14#, 16#e9#, 16#69#, 16#4f#, 16#a4#,
      16#f9#, 16#40#, 16#b2#, 16#0f#, 16#36#, 16#a5#, 16#40#, 16#01#,
      16#d3#, 16#03#, 16#2c#, 16#c7#, 16#06#, 16#9b#, 16#17#, 16#e7#);

    Key_Pair : Key_Pair_Type;
  begin
    From_Private_Key (Key_Pair, secp256k1, PRIVATE_KEY);

    Put_Line ("Sign and verify");
    declare Signature : Byte_Array := Sign (Key_Pair, MESSAGE); begin
      Put_Line ("Signature Bytes   : " & Image (Signature));
      Put_Line ("Signature base64  : " & To_String (Encode (Signature)));
      Put_Line ("Verfication Status: " & Boolean'Image (Verify (Key_Pair, Signature, Message)));
    end;
    Put_Line ("");
  end;

begin
  Test_Generate_Key_Pair;
  Test_Derive_Public_Key;
  Test_Sign_And_Verify;
end;
