
with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Byte_Array_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  function Name (T : TC) return Message_String is (Aunit.Format ("Bitcoin.Byte_Array"));

  overriding procedure Register_Tests (T : in out TC);

  procedure Test_Image               (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_To_String           (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_To_Byte_Array       (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Count_Leading_Zeros (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Trim_Leading_Zeros  (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Is_Zero             (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Is_One              (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_To_Natural          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Bignum_Conversion   (Test : in out Test_Cases.Test_Case'Class);
end;
