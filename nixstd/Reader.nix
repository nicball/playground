{ Contract, Function }:

with Contract;

rec {

  reader-c = r-c: a-c: function-c r-c a-c;

  run-reader = r: m: m r;

  functor.map = f: m: Function.compose f m;

  applicative = {
    inherit functor;
    pure = a: x: a;
    apply = mf: m: r: run-reader r mf (run-reader r m);
  };

  monad = {
    inherit applicative;
    bind = m: f: r: run-reader r (f (run-reader r m));
  };

}
