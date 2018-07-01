with Bitcoin.Data.Stacks_Tests;

package body Bitcoin.Data.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Stacks_Test_Case : aliased Bitcoin.Data.Stacks_Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Stacks_Test_Case'Access);
    return Result'Access;
  end Suite;
end;
