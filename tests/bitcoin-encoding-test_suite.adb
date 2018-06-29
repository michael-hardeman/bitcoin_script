with Bitcoin.Encoding.Base_64_Tests;
with Bitcoin.Encoding.Base_58_Tests;

package body Bitcoin.Encoding.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Base_64_Encoding_Test_Case : aliased Bitcoin.Encoding.Base_64_Tests.TC;
  Base_58_Encoding_Test_Case : aliased Bitcoin.Encoding.Base_58_Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Base_64_Encoding_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, Base_58_Encoding_Test_Case'Access);
    return Result'Access;
  end Suite;
end;
