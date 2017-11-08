package body Bitcoin.Crypto.RIPEMD160 is

  procedure Ignore (Result : in Byte_Access) is begin null; end;

  ------------
  -- Digest --
  ------------
  function Digest (B : in Byte_Array) return Byte_Array is
    Output : Byte_Array (1 .. 20) := (others => 0);
  begin
    Ignore (Bitcoin.API.OpenSSL.RIPEMD160 (
      B (B'First)'Address,
      Size_T (B'Length),
      Output (Output'First)'Address));
    return Output;
  end;
end;
