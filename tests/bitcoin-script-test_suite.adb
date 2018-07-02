with Bitcoin.Script.Tests;
with Bitcoin.Script.Op_Code_Tests;

package body Bitcoin.Script.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Script_Test_Case  : aliased Bitcoin.Script.Tests.TC;
  Op_Code_Test_Case : aliased Bitcoin.Script.Op_Code_Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Script_Test_Case'Access);
    AUnit.Test_Suites.Add_Test (Result'Access, Op_Code_Test_Case'Access);
    return Result'Access;
  end Suite;
end;
