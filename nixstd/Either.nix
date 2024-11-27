{ Contract, Applicative, Function }:

with Contract;

rec {

  either-c = a-c: b-c: or-c (attrs-spec-c { left = a-c; }) (attrs-spec-c { right = b-c; });

  left = a: { left = a; };

  right = b: { right = b; };

  is-left = m: m ? left;

  is-right = m: m ? right;

  either = f: g: m:
    if is-left m then f m.left else g m.right;

  functor.map = f: either left (r: right (f r));

  applicative = {
    inherit functor;
    pure = right;
    apply = mf: ma: either left (f: either left (Function.compose right f) ma) mf;
  };

  monad = {
    inherit applicative;
    bind = ma: f: either left f ma;
  };

  first-semigroup.op = a: b: either (Function.const b) right a;

  monad-trans = base-monad: {
    inherit base-monad;
    applicative = Applicative.compose base-monad.applicative applicative;
    bind = bea: f: # m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
      base-monad.bind bea (either (Function.compose base-monad.applicative.pure left)
                                  f);
  };

}

