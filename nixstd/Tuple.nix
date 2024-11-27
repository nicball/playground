{ Contract }:

with Contract;

rec {

  tuple-c = a-c: b-c: tuple:
    if builtins.length (list-c any-c tuple) != 2
      then throw "tuple-c: contract violation"
      else [ (a-c (builtins.elemAt tuple 0)) (b-c (builtins.elemAt tuple 1)) ];

  make-tuple = a: b: [ a b ];

  first = t: builtins.elemAt t 0;

  second = t: builtins.elemAt t 1;

  tuple = f: t: f (first t) (second t);

  functor.map = f: t: make-tuple (first t) (f (second t));

}
