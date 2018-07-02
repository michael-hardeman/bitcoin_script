with Ada.Text_IO; use Ada.Text_IO;

package body Bitcoin.Script is
  use Byte_Array_Stacks;

  ------------
  -- Parser --
  ------------
  package body Parser is
    Program_Counter : Natural := Script'First - 1;

    function  At_EOS  return Boolean is (Program_Counter >= Script'Last);
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
  procedure Evaluate (
    Script          : in  Byte_Array; 
    Primary_Stack   : out Stack_Type; 
    Secondary_Stack : out Stack_Type)
  is
    package Script_Parser is new Parser (Script); use Script_Parser;
    
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
        -- Put_Line ("Push Data: " & Byte'Image (To_Byte(Opcode)));
        Push_Bytes_To_Stack (Primary_Stack, Positive (To_Byte (Opcode)));
        return;
      end if;
      
      -- Put_Line ("Evaluate Opcode: " & Opcode_Kind'Image (Opcode));

      case Opcode is

        when Disabled_Opcode_Kind => raise Disabled_Opcode;
        when Reserved_Opcode_Kind => raise Reserved_Opcode;
        when Ignored_Opcode_Kind  => null;

        ----------
        -- Data --
        ----------
        -- An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
        when OP_0 => Push (Primary_Stack, (1 .. 4 => 16#00#));

        -- The number -1 is pushed onto the stack.
        when OP_1NEGATE => Push (Primary_Stack, (1 .. 4 => 16#FF#));

        -- The number in the word name (1-16) is pushed onto the stack.
        when OP_1 .. OP_16 => 
          Push (Primary_Stack, (1 .. 3 => 16#00#, 4 => (To_Byte (Opcode) - (To_Byte (OP_1) - 16#01#))));

        -- The next byte contains the number of bytes to be pushed onto the stack.
        when OP_PUSHDATA1 => Push_Bytes_To_Stack (Primary_Stack, Positive (To_Byte (Next)));

        -- The next two bytes contain the number of bytes to be pushed onto the stack.
        when OP_PUSHDATA2 => Push_Bytes_To_Stack (Primary_Stack, Positive (Combine (Next, Next)));

        -- The next four bytes contain the number of bytes to be pushed onto the stack.
        when OP_PUSHDATA4 => Push_Bytes_To_Stack (Primary_Stack, Positive (Combine (Next, Next, Next, Next)));

        ------------------
        -- Flow Control --
        ------------------
        -- Does nothing.
        when OP_NOP => null;

        -- OP_IF: If the top stack value is not False, the statements are executed. The top stack value is removed.
        -- OP_NOTIF: If the top stack value is False, the statements are executed. The top stack value is removed.
        when OP_IF .. OP_NOTIF => Evaluate_If_Else_Block;

        -- OP_ELSE: If the preceding OP_IF or OP_NOTIF or OP_ELSE was not executed then these statements are 
        --          and if the preceding OP_IF or OP_NOTIF or OP_ELSE was executed then these statements are not.
        -- OP_ENDIF: Ends an if/else block. All blocks must end, or the transaction is invalid. An OP_ENDIF without 
        --           OP_IF earlier is also invalid.
        -- These are only expected inside an IF/NOTIF block
        when OP_ELSE .. OP_ENDIF => raise Unexpected_Opcode;

        -- Marks transaction as invalid if top stack value is not true.
        when OP_VERIFY => if not Is_One (Pop (Primary_Stack)) then raise Verification_Failed; end if;

        -- Marks transaction as invalid. A standard way of attaching extra data to transactions is to add a zero-value
        -- output with a scriptPubKey consisting of OP_RETURN followed by exactly one pushdata op. Such outputs are 
        -- provably unspendable, reducing their cost to the network. Currently it is usually considered non-standard 
        -- (though valid) for a transaction to have more than one OP_RETURN output or an OP_RETURN output with more 
        -- than one pushdata op.
        when OP_RETURN => raise Op_Return_Encountered;

        -----------
        -- Stack --
        -----------
        -- Puts the input onto the top of the alt stack. Removes it from the main stack.
        when OP_TOALTSTACK =>
          Push (Secondary_Stack, Pop (Primary_Stack));

        -- Puts the input onto the top of the main stack. Removes it from the alt stack.
        when OP_FROMALTSTACK =>
          Push (Primary_Stack, Pop (Secondary_Stack));

        -- Puts the number of stack items onto the stack.
        when OP_DEPTH =>
          Push (Primary_Stack, (1 => Byte (Size (Primary_Stack))));

        -- Removes the top stack item.
        when OP_DROP =>
          Pop (Primary_Stack);

        -- Removes the top two stack items.
        when OP_2DROP =>
          Pop (Primary_Stack); 
          Pop (Primary_Stack);

        -- Duplicates the top stack item.
        when OP_DUP =>
          Push (Primary_Stack, Peek (Primary_Stack));

        -- Duplicates the top two stack items.
        when OP_2DUP =>
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 1));
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 1));

        -- Duplicates the top three stack items.
        when OP_3DUP =>
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 1));
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 1));
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 1));

        -- If the top stack value is not 0, duplicate it.
        when OP_IFDUP =>
          if not Is_Zero (Peek (Primary_Stack)) then
            Push (Primary_Stack, Peek (Primary_Stack)); 
          end if;

        -- Removes the second-to-top stack item.
        when OP_NIP =>
          Delete (Primary_Stack, Top_Index (Primary_Stack) - 1);
      
        -- Copies the second-to-top stack item to the top.
        -- [... 1, 2] TOP => [... 1, 2, 1] TOP
        when OP_OVER =>
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 1));

        -- Copies the pair of items two spaces back in the stack to the front.
        -- [... 1, 2. 3] TOP => [... 1, 2, 3, 1, 2] TOP
        when OP_2OVER =>
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 2));
          Push (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 2));

        -- The top three items on the stack are rotated to the left.
        -- [... 1, 2, 3] TOP => [... 2, 3, 1] TOP
        when OP_ROT =>
          Push   (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 2));
          Delete (Primary_Stack,                     Top_Index (Primary_Stack) - 3);

        -- The fifth and sixth items back are moved to the top of the stack.
        -- [... 1, 2, 3, 4, 5, 6] TOP => [... 3, 4, 5, 6, 1, 2] TOP
        when OP_2ROT => 
          Push   (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 5));
          Delete (Primary_Stack,                     Top_Index (Primary_Stack) - 6);
          Push   (Primary_Stack, Get (Primary_Stack, Top_Index (Primary_Stack) - 5));
          Delete (Primary_Stack,                     Top_Index (Primary_Stack) - 6);

        -- The top two items on the stack are swapped.
        -- [... 1, 2] TOP => [... 2, 1] TOP
        when OP_SWAP => 
          Swap (Primary_Stack, Top_Index (Primary_Stack) - 1, Top_Index (Primary_Stack));

        -- Swaps the top two pairs of items.
        -- [... 1, 2, 3, 4] TOP => [... 3, 4, 1, 2] TOP
        when OP_2SWAP =>
          Swap (Primary_Stack, Top_Index (Primary_Stack) - 3, Top_Index (Primary_Stack) - 1);
          Swap (Primary_Stack, Top_Index (Primary_Stack) - 2, Top_Index (Primary_Stack) - 0);

        -- The item n back in the stack is copied to the top.
        when OP_PICK => 
          Push (Primary_Stack, Get (Primary_Stack, To_Natural (Pop (Primary_Stack))));

        -- The item n back in the stack is moved to the top.
        when OP_ROLL => 
          Push   (Primary_Stack, Get (Primary_Stack, To_Natural (Pop (Primary_Stack))));
          Delete (Primary_Stack,                     To_Natural (Pop (Primary_Stack)));

        -- The item at the top of the stack is copied and inserted before the second-to-top item.
        when OP_TUCK => 
          Push (Primary_Stack, Get (Primary_Stack, Top_Index(Primary_Stack)));
          Swap (Primary_Stack, Top_Index(Primary_Stack) - 2, Top_Index(Primary_Stack) - 1);

        ------------
        -- Splice --
        ------------
        -- Pushes the string length of the top element of the stack (without popping it).
        when OP_SIZE => Push (Primary_Stack, (1 => Byte (Peek (Primary_Stack)'Length)));
        
        -------------------
        -- Bitwise logic --
        -------------------
        -- Returns 1 if the inputs are exactly equal, 0 otherwise.
        when OP_EQUAL => 
          if Pop (Primary_Stack) /= Pop (Primary_Stack) then Push (Primary_Stack, (1 => 16#01#)); end if;
        
        -- Same as OP_EQUAL, but runs OP_VERIFY afterward.
        when OP_EQUALVERIFY =>
          if Pop (Primary_Stack) /= Pop (Primary_Stack) then Push (Primary_Stack, (1 => 16#01#)); end if;
          if not Is_One (Pop (Primary_Stack)) then raise Verification_Failed; end if;
        
        ----------------
        -- Arithmetic --
        ----------------
        -- 1 is added to the input.
        when OP_1ADD => null;
        
        -- 1 is subtracted from the input.
        when OP_1SUB => null;
        
        -- The input is made positive.
        when OP_ABS => null;
        
        -- If the input is 0 or 1, it is flipped. Otherwise the output will be 0.
        when OP_NOT => null;
        
        -- Returns 0 if the input is 0. 1 otherwise.
        when OP_0NOTEQUAL => null;
        
        -- The 2nd stack item  is added to the top
        when OP_ADD => null;
        
        -- The 2nd stack  item is subtracted from the top of the stack.
        when OP_SUB => null;
        
        -- If both a and b are not "" (null string), the output is 1. Otherwise 0.
        when OP_BOOLAND => null;
        when OP_BOOLOR => null;
        when OP_NUMEQUAL => null;
        when OP_NUMEQUALVERIFY => null;
        when OP_NUMNOTEQUAL => null;
        when OP_LESSTHAN => null;
        when OP_GREATERTHAN => null;
        when OP_LESSTHANOREQUAL => null;
        when OP_GREATERTHANOREQUAL => null;
        when OP_MIN => null;
        when OP_MAX => null;
        when OP_WITHIN => null;
        
        ------------
        -- Crypto --
        ------------

        when others => raise Unimplemented_Feature with Opcode_Kind'Image (Opcode);
      end case;
    end;
    
  begin
    while not At_EOS loop Evaluate_Opcode (Next); end loop;
  end;
  
  --------------
  -- Evaluate --
  --------------
  procedure Evaluate (Script : in Byte_Array) is
    Primary_Stack   : Stack_Type;
    Secondary_Stack : Stack_Type;
  begin
    Evaluate (Script, Primary_Stack, Secondary_Stack);
  end;
end;
