
package Bitcoin.Test_Utilities is

  generic
    type Definite_Type is private;
    with function "="   (Left, Right : in Definite_Type) return Boolean;
    with function Image (Item        : in Definite_Type) return String;
  procedure Assert_Definite_Equal (Expected, Actual : in Definite_Type);

  generic
    type Indefinite_Type (<>) is private;
    with function "="   (Left, Right : in Indefinite_Type) return Boolean;
    with function Image (Item        : in Indefinite_Type) return String;
  procedure Assert_Indefinite_Equal (Expected, Actual : in Indefinite_Type);

end;
