{ Contract, Semigroup }:

with Contract;

{

  monoid-c = elem-c: attrs-spec-c { semigroup = Semigroup.Semigroup-c; id = elem-c; };

  string-monoid = {
    semigroup = Semigroup.string-semigroup;
    id = "";
  };

}
