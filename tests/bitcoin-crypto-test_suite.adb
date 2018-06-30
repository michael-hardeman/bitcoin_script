with Bitcoin.Crypto.Tests;
with Bitcoin.Crypto.RIPEMD160_Tests;

package body Bitcoin.Crypto.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Crypto_Test_Case    : aliased Bitcoin.Crypto.Tests.TC;
  RIPEMD160_Test_Case : aliased Bitcoin.Crypto.Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Crypto_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, RIPEMD160_Test_Case'Access);
    return Result'Access;
  end Suite;
end;
