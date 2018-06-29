with Bitcoin.Byte_Array_Tests;

package body Bitcoin.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Byte_Array_Test_Case : aliased Bitcoin.Byte_Array_Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Byte_Array_Test_Case'Access);
    return Result'Access;
  end Suite;
end;
