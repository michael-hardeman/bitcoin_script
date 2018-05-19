with AUnit.Assertions; use AUnit.Assertions;

package body Bitcoin.Test_Utilities is
  
  ---------------------------
  -- Assert_Definite_Equal --
  ---------------------------
  procedure Assert_Definite_Equal (Expected, Actual : in Definite_Type) is begin
    Assert (Actual = Expected, "Expected: " & Image (Expected) & " but found " & Image (Actual));
  end;
  
  -----------------------------
  -- Assert_Indefinite_Equal --
  -----------------------------
  procedure Assert_Indefinite_Equal (Expected, Actual : in Indefinite_Type) is begin
    Assert (Actual = Expected, "Expected: " & Image (Expected) & " but found " & Image (Actual));
  end;
end;
