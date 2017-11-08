with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Bitcoin; use Bitcoin;
with Bitcoin.Crypto.RIPEMD160; use Bitcoin.Crypto.RIPEMD160;

procedure RIPEMD160_Test is

  -----------
  -- Types --
  -----------
  type Test_State (Message_Length : Positive; Digest_Length : Positive) is record
    Message : String     (1 .. Message_Length);
    Digest  : Byte_Array (1 .. Digest_Length);
  end record;
  type Test_State_Access is access all Test_State;
  type Test_State_Access_Array is array (Positive range <>) of Test_State_Access;

  -----------
  -- TESTS --
  -----------
  TESTS : constant Test_State_Access_Array := (
    1 => new Test_State'(Message_Length => 38, Digest_Length => 20,
      Message =>
        "Jackdaws love my big sphinx of quartz.",
      Digest => (
        16#7c#, 16#03#, 16#82#, 16#0b#, 16#46#,
        16#d6#, 16#b5#, 16#ba#, 16#0e#, 16#f5#,
        16#c5#, 16#d0#, 16#90#, 16#83#, 16#8f#,
        16#6e#, 16#53#, 16#ec#, 16#84#, 16#63#)),
    2 => new Test_State'(Message_Length => 169, Digest_Length => 20,
      Message =>
        "A purely peer-to-peer version of electronic cash would allow "  &
        "online payments to be sent directly from one party to another " &
        "without going through a financial institution.",
      Digest => (
        16#1e#, 16#d9#, 16#1d#, 16#d9#, 16#08#,
        16#46#, 16#95#, 16#f4#, 16#7f#, 16#68#,
        16#db#, 16#e7#, 16#2b#, 16#a4#, 16#56#,
        16#f2#, 16#be#, 16#36#, 16#b3#, 16#9b#)),
    3 => new Test_State'(Message_Length => 145, Digest_Length => 20,
      Message =>
        "Ada was originally designed with three overriding concerns: program reliability " &
        "and maintenance, programming as a human activity, and efficiency.",
      Digest => (
        16#fe#, 16#73#, 16#e6#, 16#26#, 16#7f#,
        16#22#, 16#3a#, 16#51#, 16#01#, 16#44#,
        16#8c#, 16#5c#, 16#b1#, 16#18#, 16#7e#,
        16#0f#, 16#2d#, 16#45#, 16#5b#, 16#aa#))
  );

  --------------
  -- Run_Test --
  --------------
  procedure Run_Test (Test : in Test_State) is
    Expected_Digest : Byte_Array := Test.Digest;
    Actual_Digest   : Byte_Array := Digest (Test.Message);
  begin
    Put_Line ("----------");
    Put_Line ("-- Test --");
    Put_Line ("----------");
    Put_Line ("Message  : " & Test.Message);
    Put_Line ("Expected : " & Image (Expected_Digest));
    Put_Line ("Actual   : " & Image (Actual_Digest));
  end;

  ---------------
  -- Run_Tests --
  ---------------
  procedure Run_Tests (Tests : in Test_State_Access_Array) is
  begin
    for Test of Tests loop
      Run_Test (Test.all);
    end loop;
  end;

  procedure Free_Test is new Ada.Unchecked_Deallocation (Test_State, Test_State_Access);

  ----------------
  -- Free_Tests --
  ----------------
  procedure Free_Tests (Tests : in Test_State_Access_Array) is
  begin
    for I in Tests'Range loop
      declare Test : Test_State_Access := Tests (I); begin
        Free_Test (Test);
      end;
    end loop;
  end;
begin
  Run_Tests (TESTS);
  Free_Tests (TESTS);
end;
