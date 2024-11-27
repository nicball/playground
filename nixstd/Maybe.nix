{ Contract }:

with Contract;

rec {

  maybe-c = a-c: or-c (attrs-spec-c { just = a-c; }) (attrs-spec-c { nothing = null-c; });

  nothing = { nothing = null; };

  just = a: { just = a; };

  is-just = m: m ? just;

  is-nothing = m: m ? nothing;

  maybe = default: f: m:
    if is-just m then f m.just else default;

}
