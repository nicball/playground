{}:

rec {

  make-contract-from-predicate = name: pred: x:
    if pred x then x else throw (name + ": contract violation");

  any-c = x: x;

  none-c = x: throw "none-c: contract violation";

  poly-c = c: c any-c;

  or-c = a-c: b-c: x:
    let a = builtins.tryEval (a-c x); in
    if a.success then a.value else b-c x;

  int-c = make-contract-from-predicate "int-c" builtins.isInt;

  bool-c = make-contract-from-predicate "bool-c" builtins.isBool;

  float-c = make-contract-from-predicate "float-c" builtins.isFloat;

  null-c = make-contract-from-predicate "null-c" builtins.isNull;

  path-c = make-contract-from-predicate "path-c" builtins.isPath;

  string-c = make-contract-from-predicate "string-c" builtins.isString;

  function-c = arg-c: result-c: f: arg: result-c (f (arg-c arg));

  list-c = elem-c: list:
    builtins.map elem-c (make-contract-from-predicate "list-c" builtins.isList list);

  attrs-c = key-c: attrs:
    builtins.mapAttrs (k: v: key-c k v) (make-contract-from-predicate "attrs-c" builtins.isAttrs attrs);

  attrs-at-least-c = spec: attrs:
    let
      mandatory-keys = builtins.attrNames spec;
      all-present = builtins.all (k: attrs ? ${k}) mandatory-keys;
    in
    if builtins.isAttrs attrs && all-present
      then builtins.mapAttrs (k: v: (spec.${k} or (x: x)) v) attrs
      else throw "attrs-at-least-c: contract violation";

  attrs-at-most-c = spec: attrs:
    let
      all-allowed = builtins.all (k: spec ? ${k}) (builtins.attrNames attrs);
    in
    if builtins.isAttrs attrs && all-allowed
      then builtins.mapAttrs (k: v: spec.${k} v) attrs
      else throw "attrs-at-most-c: contract violation";

  attrs-spec-c = spec: attrs: attrs-at-least-c spec (attrs-at-most-c spec attrs);

}
