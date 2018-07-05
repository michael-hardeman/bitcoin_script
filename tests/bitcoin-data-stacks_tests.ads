with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Test_Cases);

package Bitcoin.Data.Stacks_Tests is

  type TC is new Test_Cases.Test_Case with null record;

  function Name (T : TC) return Message_String is (AUnit.Format ("Bitcoin.Data.Stacks"));

  overriding procedure Register_Tests (T : in out TC);

  procedure Test_Top_Index     (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Size          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Is_Empty      (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Get           (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Peek          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Push          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Pop           (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Pop_Procedure (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Swap          (Test : in out Test_Cases.Test_Case'Class);
  procedure Test_Delate        (Test : in out Test_Cases.Test_Case'Class);
end;
