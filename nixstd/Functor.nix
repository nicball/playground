{ Contract, Function }:

with Contract;

rec {

  functor-c = f-c: attrs-spec-c { map = poly-c (a-c: poly-c (b-c: function-c (function-c a-c b-c) (function-c (f-c a-c) (f-c b-c)))); };

  id.map = Function.id;

  compose = f: g: {
    map = Function.compose f.map g.map;
  };

  composes = builtins.foldl' compose id;

}
