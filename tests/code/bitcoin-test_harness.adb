
with AUnit.Run;
with AUnit.Reporter.Text;
with Bitcoin.Test_Suite;
with Bitcoin.Data.Stacks.Test_Suite;
with Bitcoin.Encoding.Base64.Test_Suite;
with Bitcoin.Encoding.Base58.Test_Suite;
with Bitcoin.Crypto.Test_Suite;
with Bitcoin.Crypto.RIPEMD160.Test_Suite;
with Bitcoin.Script.Test_Suite;

procedure Bitcoin.Test_Harness is

  procedure Run_Tests           is new AUnit.Run.Test_Runner (Bitcoin.Test_Suite.Suite);
  -- procedure Run_Stack_Tests     is new AUnit.Run.Test_Runner (Bitcoin.Data.Stacks.Test_Suite.Suite);
  -- procedure Run_Base64_Tests    is new AUnit.Run.Test_Runner (Bitcoin.Encoding.Base64.Test_Suite.Suite);
  -- procedure Run_Base58_Tests    is new AUnit.Run.Test_Runner (Bitcoin.Encoding.Base58.Test_Suite.Suite);
  -- procedure Run_Crypto_Tests    is new AUnit.Run.Test_Runner (Bitcoin.Crypto.Test_Suite.Suite);
  -- procedure Run_Ripemd160_Tests is new AUnit.Run.Test_Runner (Bitcoin.Crypto.RIPEMD160.Test_Suite.Suite);
  -- procedure Run_Script_Tests    is new AUnit.Run.Test_Runner (Bitcoin.Script.Test_Suite.Suite);

  Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
  Reporter.Set_Use_ANSI_Colors (True);

  Run_Tests           (Reporter);
  -- Run_Stack_Tests     (Reporter);
  -- Run_Base64_Tests    (Reporter);
  -- Run_Base58_Tests    (Reporter);
  -- Run_Crypto_Tests    (Reporter);
  -- Run_Ripemd160_Tests (Reporter);
  -- Run_Script_Tests    (Reporter);
end;
