
with AUnit.Run;
with AUnit.Reporter.Text;
pragma Elaborate_All (AUnit);
pragma Elaborate_All (AUnit.Run);
pragma Elaborate_All (AUnit.Reporter);
pragma Elaborate_All (AUnit.Reporter.Text);

with Bitcoin.Test_Suite;
with Bitcoin.Data.Test_Suite;
with Bitcoin.Encoding.Test_Suite;
with Bitcoin.Crypto.Test_Suite;
-- with Bitcoin.Script.Test_Suite;

procedure Bitcoin.Test_Harness is

  procedure Run_Bitcoin_Tests          is new AUnit.Run.Test_Runner (Bitcoin.Test_Suite.Suite);
  procedure Run_Bitcoin_Data_Tests     is new AUnit.Run.Test_Runner (Bitcoin.Data.Test_Suite.Suite);
  procedure Run_Bitcoin_Encoding_Tests is new AUnit.Run.Test_Runner (Bitcoin.Encoding.Test_Suite.Suite);
  procedure Run_Bitcoin_Crypto_Tests   is new AUnit.Run.Test_Runner (Bitcoin.Crypto.Test_Suite.Suite);
  -- procedure Run_Bitcoin_Script_Tests   is new AUnit.Run.Test_Runner (Bitcoin.Script.Test_Suite.Suite);

  Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
  Reporter.Set_Use_ANSI_Colors (True);

  Run_Bitcoin_Tests          (Reporter);
  Run_Bitcoin_Data_Tests     (Reporter);
  Run_Bitcoin_Encoding_Tests (Reporter);
  Run_Bitcoin_Crypto_Tests   (Reporter);
  -- Run_Bitcoin_Script_Tests   (Reporter);
end;
