{ Contract }:

with Contract;

{

  semigroup-c = elem-c: attrs-spec-c { op = function-c elem-c (function-c elem-c elem-c); };

  string-semigroup.op = a: b: a ++ b;

}
