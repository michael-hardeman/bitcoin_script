with Ada.Text_IO; use Ada.Text_IO;
with Bitcoin.Data.Stacks;

procedure Stacks_Test is
  package String_Stacks is new Bitcoin.Data.Stacks (Positive, String, "=");
  use String_Stacks;

  Stack : Stack_Type;
begin
  Put_Line ("Pushing: Foo"); Push (Stack, "Foo");
  Put_Line ("Pushing: Bar"); Push (Stack, "Bar");
  Put_Line ("Pushing: Baz"); Push (Stack, "Baz");
  Put_Line ("Popping: " & Pop (Stack));
  Put_Line ("Popping: " & Pop (Stack));
  Put_Line ("Popping: " & Pop (Stack));
end;
