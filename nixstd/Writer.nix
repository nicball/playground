{ Contract, Tuple }:

with Contract;

rec {

  writer-c = log-c: result-c: Tuple.tuple-c log-c result-c;

  make-writer = Tuple.make-tuple;

  writer = Tuple.tuple;

  writer-log = writer Function.const;

  writer-result = writer (Function.const Function.id);

  functor = Tuple.functor;

  applicative = log-monoid: {
    inherit functor;
    pure = make-writer log-monoid.id;
    apply = mf: m: make-writer (log-monoid.semigroup.op (write-log mf) (write-log m)) (writer-result mf (writer-result m));
  };

  monad = {
    inherit applicative;
    bind = m: f: let a = f (writer-result m); in make-writer (log-monoid.semigroup.op (write-log m) (writer-log a)) (writer-result a);
  };

}
