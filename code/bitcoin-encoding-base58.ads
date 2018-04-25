with Bitcoin.Encoding.Generic_Encoder;
with Bitcoin.Encoding.Base58_Characters; use Bitcoin.Encoding.Base58_Characters;

package Bitcoin.Encoding.Base58 is new Bitcoin.Encoding.Generic_Encoder (
  Encoded_Character    => Encoded_Character,
  To_Character         => To_Character,
  To_Encoded_Character => To_Encoded_Character);
