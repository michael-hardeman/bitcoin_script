with Bitcoin.Byte_Array_Tests;

package body Bitcoin.Test_Suite is

  Result : aliased AUnit.Test_Suites.Test_Suite;

  Byte_Array_Test_Case : aliased Bitcoin.Byte_Array_Tests.TC;

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
  begin
    AUnit.Test_Suites.Add_Test (Result'Access, Byte_Array_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, To_String_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, To_Byte_Array_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Count_Leading_Zeros_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Trim_Leading_Zeros_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Is_Zero_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, Is_One_Test_Case'Access);
    -- AUnit.Test_Suites.Add_Test (Result'Access, To_Natural_Tests_Test_Case'Access);

    return Result'Access;
  end Suite;
end;
