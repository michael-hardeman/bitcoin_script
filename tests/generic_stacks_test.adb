with Generic_Stacks;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Generic_Stacks_Test is
  package String_Stacks is new Generic_Stacks (Positive, String, "=");
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
