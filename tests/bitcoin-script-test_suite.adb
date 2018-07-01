with Bitcoin.Script.Tests;

package body Bitcoin.Script.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Script_Test_Case : aliased Bitcoin.Script.Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Script_Test_Case'Access);
    return Result'Access;
  end Suite;
end;
