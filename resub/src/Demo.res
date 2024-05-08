type name = string

type literal =
  | LitInt(int)
  | LitBool(bool)

type rec term =
  | Ref(name)
  | Lam(name, term)
  | App(term, term)
  | Lit(literal)
  | Rcd(Dict.t<term>)
  | Sel(term, name)
  | Let(name, term, term)
  
type prim_ty =
  | PrimInt
  | PrimBool

type rec ty =
  | Prim(prim_ty)
  | Intersection(ty, ty)
  | Union(ty, ty)
  | Function(ty, ty)
  | Record(Dict.t<ty>)
  | Recursive(name, ty)
  | TyVar(name)
  | Top
  | Bot
  
type rec simple_ty =
  | STyVar(name, var_state)
  | SPrim(prim_ty)
  | SFunction(simple_ty, simple_ty)
  | SRecord(Dict.t<simple_ty>)
and var_state = {
  level: int,
  mutable lower_bounds: list<simple_ty>,
  mutable upper_bounds: list<simple_ty>,
}

type type_scheme =
  | Simple_ty(simple_ty)
  | Poly(int, simple_ty)

type polarity = Positive | Negative

type polar_var = (name, polarity)

type context = Context(list<(name, type_scheme)>)

let neg = polarity => switch polarity {
| Positive => Negative
| Negative => Positive
}

exception Impossible

exception Name_not_found(name)

let rec find = (Context(context), var) =>
  switch context {
  | list{} => raise(Name_not_found(var))
  | list{(name, ty), ..._} if name === var => ty
  | list{_, ...rest} => find(Context(rest), var)
  }
  
let extend = (Context(context), name, ty) => Context(list{(name, ty), ...context})

exception Type_error(string)

let err = (str) => raise(Type_error(str))

let fresh_var = {
  let _fresh_var_counter = ref(0)
  let fresh_var = (prefix, level) => {
    let n = _fresh_var_counter.contents
    _fresh_var_counter := n + 1
    let prefix = prefix->String.replaceRegExp(Re.fromString("\.[0-9]+$"), "")
    STyVar(`${prefix}.${n->Int.toString}`, { level, lower_bounds: list{}, upper_bounds: list{} })
  }
  fresh_var
}

let rec get_level = type_scheme =>
  switch type_scheme {
  | Simple_ty(SPrim(_)) => 0
  | Simple_ty(SFunction(a, b)) => Math.Int.max(a->Simple_ty->get_level, b->Simple_ty->get_level)
  | Simple_ty(SRecord(fields)) => fields->Dict.mapValues(t => t->Simple_ty->get_level)->Dict.valuesToArray->Array.reduce(0, Math.Int.max)
  | Simple_ty(STyVar(_, vs)) => vs.level
  | Poly(level, _) => level
  }

let rec freshen_above = {
  let rec _freshen_above = (simple_ty, level, cache) =>
    switch simple_ty {
    | SPrim(_) => simple_ty
    | SFunction(a, b) => SFunction(a->_freshen_above(level, cache), b->_freshen_above(level, cache))
    | SRecord(fields) => SRecord(fields->Dict.mapValues(_freshen_above(_, level, cache)))
    | STyVar(name, vs) =>
      if level < vs.level {
        switch cache->Dict.get(name) {
        | Some(new_var) => new_var
        | None =>
          let new_var = fresh_var(name, level)
          cache->Dict.set(name, new_var)
          switch new_var {
          | STyVar(_, nvs) =>
            nvs.lower_bounds = vs.lower_bounds->List.map(_freshen_above(_, level, cache))
            nvs.upper_bounds = vs.upper_bounds->List.map(_freshen_above(_, level, cache))
          | _ => raise(Impossible)
          }
          new_var
        }
      }
      else {
        simple_ty
      }
    }
  (a, b) => _freshen_above(a, b, Dict.make())
}

and instantiate_at = (type_scheme, level) => {
  level->ignore
  switch type_scheme {
  | Poly(level, body) => body->freshen_above(level)
  | Simple_ty(simple_ty) => simple_ty
  }
}

// let f = \x -> x
// x : 'a.1
// \x -> x : 'a.1 -> 'a.1
// f : Poly(0, 'a.1 -> 'a.1)
// f : 'b.0 -> 'b.0

let rec extrude = (simple_ty, polarity, level, cache) =>
  if simple_ty->Simple_ty->get_level <= level { simple_ty } else {
    switch simple_ty {
    | SPrim(_) => simple_ty
    | SFunction(a, b) => SFunction(a->extrude(polarity->neg, level, cache), b->extrude(polarity, level, cache))
    | SRecord(fields) => SRecord(fields->Dict.mapValues(extrude(_, polarity, level, cache)))
    | STyVar(name, vs) =>
      cache->Dict.get(name)->Option.getOr({
        let new_var = fresh_var(name, level)
        cache->Dict.set(name, new_var)
        switch polarity {
        | Positive =>
          vs.upper_bounds = list{new_var, ...vs.upper_bounds}
          switch new_var {
          | STyVar(_, nvs) => nvs.upper_bounds = vs.upper_bounds->List.map(extrude(_, polarity, level, cache))
          | _ => raise(Impossible)
          }
        | Negative =>
          vs.lower_bounds = list{new_var, ...vs.lower_bounds}
          switch new_var {
          | STyVar(_, nvs) => nvs.lower_bounds = vs.lower_bounds->List.map(extrude(_, polarity, level, cache))
          | _ => raise(Impossible)
          }
        }
        new_var
      })
    }
  }

let rec show_simple_ty = simple_ty =>
  switch simple_ty {
  | SPrim(PrimInt) => "int"
  | SPrim(PrimBool) => "bool"
  | SRecord(fields) => "{ " ++ fields->Dict.toArray->Array.map(((name, ty)) => `${name}: ${show_simple_ty(ty)}; `)->Array.reduce("", (a, b) => a ++ b) ++ "}"
  | SFunction(a, b) => `(${show_simple_ty(a)} -> ${show_simple_ty(b)})`
  | STyVar(name, vs) => `${name}(${vs.level->Int.toString})`
  }

let constrain = {
  let seen = Hashtbl.create(1)
  let rec constrain = (ty1, ty2) => {
    switch seen->Hashtbl.find_opt((ty1, ty2)) {
    | Some(()) => ()
    | None =>
      seen->Hashtbl.add((ty1, ty2), ())
      really_constrain(ty1, ty2)
    }
  }
  and really_constrain = (ty1, ty2) => {
    switch (ty1, ty2) {
    | (SPrim(a), SPrim(b)) if a == b => ()
    | (SFunction(a0, b0), SFunction(a1, b1)) =>
      constrain(b0, b1)
      constrain(a1, a0)
    | (SRecord(fields1), SRecord(fields2)) =>
      fields2->Dict.forEachWithKey((ty2, name2) => {
        switch fields1->Dict.get(name2) {
        | None => err(`${name2} not found in record`)
        | Some(ty1) => constrain(ty1, ty2)
        }
      })
    | (STyVar(_, vs1), _) if ty2->Simple_ty->get_level <= ty1->Simple_ty->get_level =>
      vs1.upper_bounds = list{ty2, ...vs1.upper_bounds}
      vs1.lower_bounds->List.forEach(constrain(_, ty2))
    | (_ , STyVar(_, vs2)) if ty1->Simple_ty->get_level <= ty2->Simple_ty->get_level =>
      vs2.lower_bounds = list{ty1, ...vs2.lower_bounds}
      vs2.upper_bounds->List.forEach(constrain(ty1, _))
    | (STyVar(_, _), _) =>
      constrain(ty1, ty2->extrude(Negative, ty1->Simple_ty->get_level, Dict.make()))
    | (_, STyVar(_, _)) =>
      constrain(ty1->extrude(Positive, ty2->Simple_ty->get_level, Dict.make()), ty2)
    | _ => err(`cannot constain ${ty1->show_simple_ty} to ${ty2->show_simple_ty}`)
    }
  }
  constrain
}

let rec infer = (context, level, term) =>
  switch term {
  | Lit(LitInt(_)) => SPrim(PrimInt)
  | Lit(LitBool(_)) => SPrim(PrimBool)
  | Ref(name) => context->find(name)->instantiate_at(level)
  | Rcd(fields) => SRecord(fields->Dict.mapValues(infer(context, level, _)))
  | Sel(rcd, name) =>
    let res = fresh_var(name, level)
    constrain(infer(context, level, rcd), SRecord(Dict.fromArray([(name, res)])))
    res
  | Lam(param, body) =>
    let param_ty = fresh_var(param, level)
    SFunction(param_ty, infer(context->extend(param, Simple_ty(param_ty)), level, body))
  | App(f, a) =>
    let res = fresh_var("app", level)
    constrain(infer(context, level, f), SFunction(infer(context, level, a), res))
    res
  | Let(v, e, body) =>
    let v_ty = fresh_var(v, level + 1)
    let e_ty = infer(context->extend(v, Simple_ty(v_ty)), level + 1, e)
    constrain(e_ty, v_ty)
    infer(context->extend(v, Poly(level, v_ty)), level, body)
  }

let coalesce_ty = {
  let recursive = Hashtbl.create(5)
  let rec coalesce_ty = (inflight, simple_ty, polarity) =>
    switch simple_ty {
    | SPrim(p) => Prim(p)
    | SFunction(a, b) => Function(coalesce_ty(inflight, a, neg(polarity)), coalesce_ty(inflight, b, polarity))
    | SRecord(fields) => Record(fields->Dict.mapValues(coalesce_ty(inflight, _, polarity)))
    | STyVar(name, vs) =>
      let pname = (name, polarity)
      if inflight->List.find(v => v == pname)->Option.isSome {
        recursive->Hashtbl.add(pname, ())
        TyVar(name)
      }
      else {
        let bounds = switch polarity { | Positive => vs.lower_bounds | Negative => vs.upper_bounds }
        let coalesced_bounds = bounds->List.map(coalesce_ty(list{pname, ...inflight}, _, polarity))
        let conn = switch polarity { | Positive => (a, b) => Union(a, b) | Negative => (a, b) => Intersection(a, b) }
        let res = coalesced_bounds->List.reduce(TyVar(name), conn)
        if recursive->Hashtbl.find_opt(pname)->Option.isSome {
          Recursive(name, res)
        }
        else {
          res
        }
      }
    }
  coalesce_ty(list{}, _, Positive)
}

let context = Context(list{
  // if : bool -> a -> a -> a
  ("if", {
    let var = fresh_var("a", 1)
    Poly(0, SFunction(SPrim(PrimBool), SFunction(var, SFunction(var, var))))
  }),
  // + : int -> int -> int
  ("+", Simple_ty(SFunction(SPrim(PrimInt), SFunction(SPrim(PrimInt), SPrim(PrimInt)))))
})

let if_ = (c, t, e) => App(App(App(Ref("if"), c), t), e)
let add = (a, b) => App(App(Ref("+"), a), b)

// program =
// let f = \x -> \y ->
//   if x.cond
//     then x.value + y.value
//     else x.default
// in f

let somercd = Rcd(Dict.fromArray([ ("cond", Lit(LitBool(false))), ("value", Lit(LitInt(42))), ("default", Lit(LitInt(1))) ]))
let program =
  Let(
    "f",
    Lam("x",
      Lam("y",
        if_(
          Sel(Ref("x"), "cond"),
          add(Sel(Ref("x"), "value"), Sel(Ref("y"), "value")),
          Sel(Ref("x"), "default")))),
    Ref("f"))

let rec show_ty = ty =>
  switch ty {
  | Prim(PrimInt) => "int"
  | Prim(PrimBool) => "bool"
  | Record(fields) => "{ " ++ fields->Dict.toArray->Array.map(((name, ty)) => `${name}: ${show_ty(ty)}; `)->Array.reduce("", (a, b) => a ++ b) ++ "}"
  | Union(a, b) => `(${show_ty(a)} | ${show_ty(b)})`
  | Intersection(a, b) => `(${show_ty(a)} & ${show_ty(b)})`
  | Top => "top"
  | Bot => "bottom"
  | TyVar(name) => name
  | Function(a, b) => `(${show_ty(a)} -> ${show_ty(b)})`
  | Recursive(name, body) => `(fix ${name} in ${show_ty(body)})`
  }

let ty = coalesce_ty(infer(context, 0, program))
Console.log(show_ty(ty))
