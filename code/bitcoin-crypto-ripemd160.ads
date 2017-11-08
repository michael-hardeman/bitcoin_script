with Bitcoin.API.OpenSSL;

package Bitcoin.Crypto.RIPEMD160 is
  function Digest (B : in Byte_Array) return Byte_Array;
  function Digest (S : in String)     return Byte_Array is (Digest (To_Byte_Array (S)));
end;
