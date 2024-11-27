{ Contract, Either, Reader, Function, Writer, Tuple, Functor, Applicative, Maybe }:

with Contract;

rec {

  parsec-c = result-c: function-c string-c (Either.either-c string-c (Tuple.tuple-c string-c result-c));

  run-parsec = input: p: p input;

  functor = Functor.composes [ Reader.functor Either.functor Tuple.functor ];

  applicative = {
    pure = a: input: Either.right [ input a ];
    apply = mf: ma: input:
      Either.either Either.left
                    (tf: Either.either Either.left
                                       (ta: Either.right (Tuple.make-tuple (Tuple.first ta)
                                                                           (Tuple.second tf (Tuple.second ta))))
                                       (run-parsec (Tuple.first tf) ma))
                    (run-parsec input mf);
  };

  semigroup.op = Applicative.lift-2 Reader.applicative Either.first-semigroup.op;

  monoid = {
    inherit semigroup;
    id = Function.const (Either.left "always fail");
  };

  some = v: let cons = x: xs: [ x ] ++ xs; in
    Applicative.lift-2 applicative cons v (many v);

  many = v: semigroup.op (some v) (applicative.pure []);

  monad = {
    inherit applicative;
    bind = m: f: input:
      Either.either Either.left (t: run-parsec (Tuple.first t) (f (Tuple.second t))) (run-parsec input m);
  };

  eof = input: if input == "" then Either.right [ "" null ] else Either.left "expected EOF";

  empty = input: Either.right [ input null ];

  match-char-if = pred: input:
    let head = builtins.substring 0 1 input; tail = builtins.substring 1 (-1) input; in
    if builtins.stringLength input == 0 || !(pred head)
      then Either.left ''match-char-if: failed for "${head}"''
      else Either.right [ tail head ];

  exactly = prefix: input:
    let len = builtins.stringLength prefix; in
    if builtins.stringLength input < len || builtins.substring 0 len input != prefix
      then Either.left ''expected "${prefix}"''
      else Either.right [ (builtins.substring len (-1) input) prefix ];

  optional = p: semigroup.op (functor.map Maybe.just p) (applicative.pure Maybe.nothing);

  space = match-char-if (c: c == " " || c == "\t" || c == "\n");

  numeric =
    let
      to-int = n: { "0" = 0; "1" = 1; "2" = 2; "3" = 3; "4" = 4; "5" = 5; "6" = 6; "7" = 7; "8" = 8; "9" = 9; }.${n};
    in
    functor.map to-int (match-char-if (c: c == "0" || c == "1" || c == "2" || c == "3" || c == "4" || c == "5" || c == "6" || c == "7" || c == "8" || c == "9"));

  number =
    let
      to-int = builtins.foldl' (a: n: a * 10 + n) 0;
    in
    functor.map to-int (some numeric);

  sep-by = sep: p:
    monad.bind p (a-p:
      monad.bind (optional (Applicative.right applicative sep (sep-by sep p)))
        (Maybe.maybe (applicative.pure [ a-p ]) (r: applicative.pure ([ a-p ] ++ r))));

}
