
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Bitcoin.Byte_Array_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  overriding procedure Register_Tests (T : in out TC);

  function Name (T : TC) return Message_String;

  procedure Test_Image               (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_To_String           (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_To_Byte_Array       (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Count_Leading_Zeros (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Trim_Leading_Zeros  (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Is_Zero             (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Is_One              (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_To_Natural          (Test : in out Test_Cases.Test_Case'Class);
end;
