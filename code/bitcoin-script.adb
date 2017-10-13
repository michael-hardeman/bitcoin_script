package body Bitcoin.Script is
  use Byte_Array_Stacks;

  -----------------
  -- Comparisons --
  -----------------
  function Is_Zero (Bytes : in Byte_Array) return Boolean is (Bytes = (Bytes'Range => 16#00#));
  function Is_One  (Bytes : in Byte_Array) return Boolean is begin
    return (for all I in Bytes'First .. Bytes'Last => Bytes (I) = 16#00#) and then Bytes (Bytes'Last) = 16#01#;
  end;

  ------------
  -- Parser --
  ------------
  package body Parser is
    Program_Counter : Positive := Script'First;

    function  At_EOS  return Boolean is (Program_Counter > Script'Last);
    function  Peek    return Byte    is (Script (Positive'Succ (Program_counter)));
    function  Current return Byte    is (Script (Program_Counter));
    function  Next    return Byte    is begin Skip; return Current; end;
    procedure Skip                   is begin Program_Counter := Positive'Succ (Program_counter); end;
    function  Assert     (Opcodes : in Opcode_Kind_Array) return Boolean is begin return (for some Opcode of Opcodes => Opcode = Peek); end;
    procedure Skip_Until (Opcodes : in Opcode_Kind_Array) is begin while not Assert (Opcodes) loop Skip; end loop; end;
    procedure Skip_Until (Opcode  : in Opcode_Kind)       is begin Skip_Until ((1 => Opcode)); end;
    procedure Ensure     (Opcodes : in Opcode_Kind_Array) is begin if not Assert (Opcodes) then raise Unexpected_Opcode; end if; end;
    procedure Ensure     (Opcode  : in Opcode_Kind)       is begin Ensure ((1 => Opcode)); end;

    -------------------
    -- Skip_If_Block --
    -------------------
    procedure Skip_If_Block is begin
      Skip_Until ((OP_IF, OP_NOTIF, OP_ELSE, OP_ENDIF));
      while Assert ((OP_IF, OP_NOTIF)) loop
        Skip;
        Skip_If_Else_Block;
        Skip_Until ((OP_IF, OP_NOTIF, OP_ELSE, OP_ENDIF));
      end loop;
    end;

    ---------------------
    -- Skip_Else_Block --
    ---------------------
    procedure Skip_Else_Block is begin
      Skip_Until ((OP_IF, OP_NOTIF, OP_ELSE, OP_ENDIF));
      while Assert ((OP_IF, OP_NOTIF, OP_ELSE)) loop
        Skip;
        Skip_If_Else_Block;
        Skip_Until ((OP_IF, OP_NOTIF, OP_ELSE, OP_ENDIF));
      end loop;
    end;

    ------------------------
    -- Skip_If_Else_Block --
    ------------------------
    procedure Skip_If_Else_Block is begin
      Skip_If_Block;
      Ensure ((OP_ELSE, OP_ENDIF));
      if Next = OP_ENDIF then return; end if;
      Skip_Else_Block;
      Ensure (OP_ENDIF);
      Skip;
    end;
  end;

  ----------------------
  -- Combine Unsigned --
  ----------------------
  function Combine (High, Low                  : in Byte)        return Unsigned_16 is (Shift_Left (Unsigned_16 (High), 8)  or Unsigned_16 (Low));
  function Combine (High, Low                  : in Unsigned_16) return Unsigned_32 is (Shift_Left (Unsigned_32 (High), 16) or Unsigned_32 (Low));
  function Combine (Highest, High, Low, Lowest : in Byte)        return Unsigned_32 is (Combine (Combine (Highest, High), Combine (Low, Lowest)));

  --------------
  -- Evaluate --
  --------------
  procedure Evaluate (Script : in Byte_Array) is
    package Script_Parser is new Parser (Script); use Script_Parser;

    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;

    -------------------------
    -- Push_Bytes_To_Stack --
    -------------------------
    procedure Push_Bytes_To_Stack (Stack : in out Stack_Type; Quantity : in Positive) is
      Accumulator : Byte_Array := (1 .. Quantity => 0);
    begin
      for I in Accumulator'Range loop Accumulator (I) := Next; end loop;
      Push (Stack, Accumulator);
    end;

    ---------------------
    -- Evaluate_Opcode --
    ---------------------
    procedure Evaluate_Opcode (Opcode : in Opcode_Kind) is

      ----------------------------
      -- Evaluate_If_Else_Block --
      ----------------------------
      procedure Evaluate_If_Else_Block is
        Top : Byte_Array := Pop (Primary_Stack);
      begin
        if ((Current = OP_IF    and then not Is_Zero (Top))
        or  (Current = OP_NOTIF and then     Is_Zero (Top))) then
          while not Assert ((OP_ELSE, OP_ENDIF)) loop Evaluate_Opcode (Next); end loop;
          Ensure ((OP_ELSE, OP_ENDIF));
          if Next = OP_ENDIF then return; end if;
          Skip_Else_Block;
          Ensure (OP_ENDIF);
          Skip;
        else
          Skip_If_Block;
          Ensure ((OP_ELSE, OP_ENDIF));
          if Next = OP_ENDIF then return; end if;
          while not Assert (OP_ENDIF) loop Evaluate_Opcode (Next); end loop;
          Ensure (OP_ENDIF);
          Skip;
        end if;
      end;

    begin
      if not Opcode'Valid then
        if not (To_Byte (Opcode) in Data_Count_Range) then raise Invalid_Opcode; end if;
        Push_Bytes_To_Stack (Primary_Stack, Positive (To_Byte (Opcode)));
        return;
      end if;

      case Opcode is

        when Disabled_Opcode_Kind => raise Disabled_Opcode;
        when Reserved_Opcode_Kind => raise Reserved_Opcode;
        when Ignored_Opcode_Kind  => null;

        ----------
        -- Data --
        ----------
        when OP_0          => Push (Primary_Stack, (1 .. 4 => 16#00#));
        when OP_1NEGATE    => Push (Primary_Stack, (1 .. 4 => 16#FF#));
        when OP_1 .. OP_16 => Push (Primary_Stack, (1 .. 3 => 16#00#, 4 => (To_Byte (Opcode) - (To_Byte (OP_1) - 16#01#))));
        when OP_PUSHDATA1  => Push_Bytes_To_Stack (Primary_Stack, Positive (To_Byte (Next)));
        when OP_PUSHDATA2  => Push_Bytes_To_Stack (Primary_Stack, Positive (Combine (Next, Next)));
        when OP_PUSHDATA4  => Push_Bytes_To_Stack (Primary_Stack, Positive (Combine (Next, Next, Next, Next)));

        ------------------
        -- Flow Control --
        ------------------
        when OP_NOP              => Skip;
        when OP_IF   .. OP_NOTIF => Evaluate_If_Else_Block;
        when OP_ELSE .. OP_ENDIF => raise Unexpected_Opcode;
        when OP_VERIFY           => if not Is_One (Pop (Primary_Stack)) then raise Verification_Failed; end if;
        when OP_RETURN           => raise Op_Return_Encountered;

        -----------
        -- Stack --
        -----------
        when others => raise Unimplemented_Feature;
      end case;
    end;
  begin
    while not At_EOS loop Evaluate_Opcode (Next); end loop;
  end;
end;
