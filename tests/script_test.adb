with Ada.Text_IO; use Ada.Text_IO;
with Bitcoin; use Bitcoin;
with Bitcoin.Script; use Bitcoin.Script;

procedure Script_Test is
  -- Example Typical Bitcoin Transaction picked at random
  -- https://blockchain.info/tx/9a88b001b405ea5b9d68b75f995184a85ac36c5b5bf6ab78f9311be75f829b91?show_adv=true
  -- TXID: 9a88b001b405ea5b9d68b75f995184a85ac36c5b5bf6ab78f9311be75f829b91

  Signature : constant Byte_Array := (
    16#30#, 16#45#, 16#02#, 16#21#, 16#00#, 16#80#, 16#00#, 16#d0#,
    16#68#, 16#56#, 16#0c#, 16#53#, 16#a8#, 16#4d#, 16#90#, 16#5e#,
    16#ea#, 16#66#, 16#24#, 16#1f#, 16#97#, 16#8e#, 16#47#, 16#af#,
    16#53#, 16#d6#, 16#9e#, 16#fc#, 16#10#, 16#14#, 16#81#, 16#55#,
    16#56#, 16#a8#, 16#36#, 16#fe#, 16#86#, 16#02#, 16#20#, 16#6e#,
    16#02#, 16#30#, 16#d6#, 16#7d#, 16#ee#, 16#5d#, 16#64#, 16#5e#,
    16#a0#, 16#45#, 16#16#, 16#44#, 16#02#, 16#70#, 16#b5#, 16#95#,
    16#9e#, 16#09#, 16#be#, 16#27#, 16#d4#, 16#56#, 16#1a#, 16#de#,
    16#a3#, 16#25#, 16#19#, 16#a0#, 16#ca#, 16#0e#, 16#d2#, 16#01#);

  Public_Key : constant Byte_Array := (
    16#03#, 16#90#, 16#94#, 16#50#, 16#cf#, 16#95#, 16#3d#, 16#b2#, 16#f4#, 16#77#, 16#d7#,
    16#c9#, 16#34#, 16#ad#, 16#78#, 16#e6#, 16#3f#, 16#2c#, 16#de#, 16#d4#, 16#ca#, 16#95#,
    16#c1#, 16#8c#, 16#ba#, 16#0b#, 16#76#, 16#0c#, 16#f0#, 16#f0#, 16#ba#, 16#74#, 16#e6#);

  Script : constant Byte_Array := (
    To_Byte (OP_DUP),
    To_Byte (OP_HASH160),
    OP_PUSHDATA (16#20#),
    16#6d#, 16#b4#, 16#7c#, 16#77#, 16#d4#,
    16#9e#, 16#e1#, 16#f5#, 16#e0#, 16#57#,
    16#66#, 16#53#, 16#f2#, 16#e2#, 16#f5#,
    16#d5#, 16#0f#, 16#c5#, 16#95#, 16#4a#,
    To_Byte (OP_EQUALVERIFY),
    To_Byte (OP_CHECKSIG));
begin
  -- Should Evaluate without raising errors
  Evaluate (
    OP_PUSHDATA (Byte (Signature'Length))  & Signature  &
    OP_PUSHDATA (Byte (Public_Key'Length)) & Public_Key &
    Script);
end;

