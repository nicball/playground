{ Contract, Function, Functor }:

with Contract;

rec {

  applicative-c = f-c: attrs-spec-c {
    functor = functor-c f-c;
    pure = poly-c (a-c: function-c a-c (f-c a-c));
    apply = poly-c (a-c: poly-c (b-c: function-c (f-c (function-c a-c b-c)) (function-c (f-c a-c) (f-c b-c))));
  };

  id = {
    functor = Functor.id;
    pure = Function.id;
    apply = Function.id;
  };

  compose = f: g: {
    functor = Functor.compose f.functor g.functor;
    pure = Function.compose f.pure g.pure;
    apply = Function.compose f.apply (f.map g.apply);
  };

  composes = builtins.foldl' compose id;

  lift-2 = applicative: f: a: b: with applicative; apply (apply (pure f) a) b;

  left = applicative: lift-2 applicative Function.const;

  lefts = applicative: as: builtins.foldl' (left applicative) (head as) (tail as);

  right = applicative: lift-2 applicative (Function.const Function.id);

  rights = applicative: as: builtins.foldl' (right applicative) (head as) (tail as);

}
