// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Core__Dict from "@rescript/core/src/Core__Dict.mjs";
import * as Core__List from "@rescript/core/src/Core__List.mjs";
import * as Core__Array from "@rescript/core/src/Core__Array.mjs";
import * as Core__Option from "@rescript/core/src/Core__Option.mjs";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

function neg(polarity) {
  if (polarity === "Positive") {
    return "Negative";
  } else {
    return "Positive";
  }
}

var Impossible = /* @__PURE__ */Caml_exceptions.create("Demo.Impossible");

var Name_not_found = /* @__PURE__ */Caml_exceptions.create("Demo.Name_not_found");

function find(_context, $$var) {
  while(true) {
    var context = _context;
    var context$1 = context._0;
    if (context$1) {
      var match = context$1.hd;
      if (match[0] === $$var) {
        return match[1];
      }
      _context = {
        TAG: "Context",
        _0: context$1.tl
      };
      continue ;
    }
    throw {
          RE_EXN_ID: Name_not_found,
          _1: $$var,
          Error: new Error()
        };
  };
}

function extend(context, name, ty) {
  return {
          TAG: "Context",
          _0: {
            hd: [
              name,
              ty
            ],
            tl: context._0
          }
        };
}

var Type_error = /* @__PURE__ */Caml_exceptions.create("Demo.Type_error");

function err(str) {
  throw {
        RE_EXN_ID: Type_error,
        _1: str,
        Error: new Error()
      };
}

var _fresh_var_counter = {
  contents: 0
};

function fresh_var(prefix, level) {
  var n = _fresh_var_counter.contents;
  _fresh_var_counter.contents = n + 1 | 0;
  var prefix$1 = prefix.replace(new RegExp("\.[0-9]+$"), "");
  return {
          TAG: "STyVar",
          _0: prefix$1 + "." + n.toString(),
          _1: {
            level: level,
            lower_bounds: /* [] */0,
            upper_bounds: /* [] */0
          }
        };
}

function get_level(type_scheme) {
  if (type_scheme.TAG !== "Simple_ty") {
    return type_scheme._0;
  }
  var fields = type_scheme._0;
  switch (fields.TAG) {
    case "STyVar" :
        return fields._1.level;
    case "SPrim" :
        return 0;
    case "SFunction" :
        return Math.max(get_level({
                        TAG: "Simple_ty",
                        _0: fields._0
                      }), get_level({
                        TAG: "Simple_ty",
                        _0: fields._1
                      }));
    case "SRecord" :
        return Core__Array.reduce(Object.values(Core__Dict.mapValues(fields._0, (function (t) {
                              return get_level({
                                          TAG: "Simple_ty",
                                          _0: t
                                        });
                            }))), 0, (function (prim0, prim1) {
                      return Math.max(prim0, prim1);
                    }));
    
  }
}

function _freshen_above(simple_ty, level, cache) {
  switch (simple_ty.TAG) {
    case "STyVar" :
        var vs = simple_ty._1;
        if (level >= vs.level) {
          return simple_ty;
        }
        var name = simple_ty._0;
        var new_var = cache[name];
        if (new_var !== undefined) {
          return new_var;
        }
        var new_var$1 = fresh_var(name, level);
        cache[name] = new_var$1;
        if (new_var$1.TAG === "STyVar") {
          var nvs = new_var$1._1;
          nvs.lower_bounds = Core__List.map(vs.lower_bounds, (function (__x) {
                  return _freshen_above(__x, level, cache);
                }));
          nvs.upper_bounds = Core__List.map(vs.upper_bounds, (function (__x) {
                  return _freshen_above(__x, level, cache);
                }));
        } else {
          throw {
                RE_EXN_ID: Impossible,
                Error: new Error()
              };
        }
        return new_var$1;
    case "SPrim" :
        return simple_ty;
    case "SFunction" :
        return {
                TAG: "SFunction",
                _0: _freshen_above(simple_ty._0, level, cache),
                _1: _freshen_above(simple_ty._1, level, cache)
              };
    case "SRecord" :
        return {
                TAG: "SRecord",
                _0: Core__Dict.mapValues(simple_ty._0, (function (__x) {
                        return _freshen_above(__x, level, cache);
                      }))
              };
    
  }
}

function freshen_above(a, b) {
  return _freshen_above(a, b, {});
}

function instantiate_at(type_scheme, level) {
  if (type_scheme.TAG === "Simple_ty") {
    return type_scheme._0;
  } else {
    return freshen_above(type_scheme._1, type_scheme._0);
  }
}

function extrude(simple_ty, polarity, level, cache) {
  if (get_level({
          TAG: "Simple_ty",
          _0: simple_ty
        }) <= level) {
    return simple_ty;
  }
  switch (simple_ty.TAG) {
    case "STyVar" :
        var vs = simple_ty._1;
        var name = simple_ty._0;
        var new_var = fresh_var(name, level);
        cache[name] = new_var;
        if (polarity === "Positive") {
          vs.upper_bounds = {
            hd: new_var,
            tl: vs.upper_bounds
          };
          if (new_var.TAG === "STyVar") {
            new_var._1.upper_bounds = Core__List.map(vs.upper_bounds, (function (__x) {
                    return extrude(__x, polarity, level, cache);
                  }));
          } else {
            throw {
                  RE_EXN_ID: Impossible,
                  Error: new Error()
                };
          }
        } else {
          vs.lower_bounds = {
            hd: new_var,
            tl: vs.lower_bounds
          };
          if (new_var.TAG === "STyVar") {
            new_var._1.lower_bounds = Core__List.map(vs.lower_bounds, (function (__x) {
                    return extrude(__x, polarity, level, cache);
                  }));
          } else {
            throw {
                  RE_EXN_ID: Impossible,
                  Error: new Error()
                };
          }
        }
        return Core__Option.getOr(cache[name], new_var);
    case "SPrim" :
        return simple_ty;
    case "SFunction" :
        return {
                TAG: "SFunction",
                _0: extrude(simple_ty._0, neg(polarity), level, cache),
                _1: extrude(simple_ty._1, polarity, level, cache)
              };
    case "SRecord" :
        return {
                TAG: "SRecord",
                _0: Core__Dict.mapValues(simple_ty._0, (function (__x) {
                        return extrude(__x, polarity, level, cache);
                      }))
              };
    
  }
}

function show_simple_ty(simple_ty) {
  switch (simple_ty.TAG) {
    case "STyVar" :
        return simple_ty._0 + "(" + simple_ty._1.level.toString() + ")";
    case "SPrim" :
        if (simple_ty._0 === "PrimInt") {
          return "int";
        } else {
          return "bool";
        }
    case "SFunction" :
        return "(" + show_simple_ty(simple_ty._0) + " -> " + show_simple_ty(simple_ty._1) + ")";
    case "SRecord" :
        return "{ " + Core__Array.reduce(Object.entries(simple_ty._0).map(function (param) {
                        return param[0] + ": " + show_simple_ty(param[1]) + "; ";
                      }), "", (function (a, b) {
                      return a + b;
                    })) + "}";
    
  }
}

var seen = Hashtbl.create(undefined, 1);

function constrain(ty1, ty2) {
  var match = Hashtbl.find_opt(seen, [
        ty1,
        ty2
      ]);
  if (match !== undefined) {
    return ;
  } else {
    Hashtbl.add(seen, [
          ty1,
          ty2
        ], undefined);
    var exit = 0;
    var exit$1 = 0;
    switch (ty1.TAG) {
      case "STyVar" :
          var vs1 = ty1._1;
          if (get_level({
                  TAG: "Simple_ty",
                  _0: ty2
                }) <= get_level({
                  TAG: "Simple_ty",
                  _0: ty1
                })) {
            vs1.upper_bounds = {
              hd: ty2,
              tl: vs1.upper_bounds
            };
            return Core__List.forEach(vs1.lower_bounds, (function (__x) {
                          constrain(__x, ty2);
                        }));
          }
          exit$1 = 3;
          break;
      case "SPrim" :
          switch (ty2.TAG) {
            case "STyVar" :
                exit$1 = 3;
                break;
            case "SPrim" :
                if (ty1._0 === ty2._0) {
                  return ;
                }
                exit = 2;
                break;
            case "SFunction" :
            case "SRecord" :
                exit = 2;
                break;
            
          }
          break;
      case "SFunction" :
          switch (ty2.TAG) {
            case "STyVar" :
                exit$1 = 3;
                break;
            case "SFunction" :
                constrain(ty1._1, ty2._1);
                return constrain(ty2._0, ty1._0);
            case "SPrim" :
            case "SRecord" :
                exit = 2;
                break;
            
          }
          break;
      case "SRecord" :
          var fields1 = ty1._0;
          switch (ty2.TAG) {
            case "STyVar" :
                exit$1 = 3;
                break;
            case "SPrim" :
            case "SFunction" :
                exit = 2;
                break;
            case "SRecord" :
                return Core__Dict.forEachWithKey(ty2._0, (function (ty2, name2) {
                              var ty1 = fields1[name2];
                              if (ty1 !== undefined) {
                                return constrain(ty1, ty2);
                              }
                              throw {
                                    RE_EXN_ID: Type_error,
                                    _1: name2 + " not found in record",
                                    Error: new Error()
                                  };
                            }));
            
          }
          break;
      
    }
    if (exit$1 === 3) {
      if (ty2.TAG === "STyVar") {
        var vs2 = ty2._1;
        if (get_level({
                TAG: "Simple_ty",
                _0: ty1
              }) <= get_level({
                TAG: "Simple_ty",
                _0: ty2
              })) {
          vs2.lower_bounds = {
            hd: ty1,
            tl: vs2.lower_bounds
          };
          return Core__List.forEach(vs2.upper_bounds, (function (__x) {
                        constrain(ty1, __x);
                      }));
        }
        exit = 1;
      } else {
        exit = 1;
      }
    }
    switch (exit) {
      case 1 :
          if (ty1.TAG === "STyVar") {
            return constrain(ty1, extrude(ty2, "Negative", get_level({
                                TAG: "Simple_ty",
                                _0: ty1
                              }), {}));
          } else {
            return constrain(extrude(ty1, "Positive", get_level({
                                TAG: "Simple_ty",
                                _0: ty2
                              }), {}), ty2);
          }
      case 2 :
          var str = "cannot constain " + show_simple_ty(ty1) + " to " + show_simple_ty(ty2);
          throw {
                RE_EXN_ID: Type_error,
                _1: str,
                Error: new Error()
              };
      
    }
  }
}

function infer(_context, level, _term) {
  while(true) {
    var term = _term;
    var context = _context;
    switch (term.TAG) {
      case "Ref" :
          return instantiate_at(find(context, term._0), level);
      case "Lam" :
          var param = term._0;
          var param_ty = fresh_var(param, level);
          return {
                  TAG: "SFunction",
                  _0: param_ty,
                  _1: infer(extend(context, param, {
                            TAG: "Simple_ty",
                            _0: param_ty
                          }), level, term._1)
                };
      case "App" :
          var res = fresh_var("app", level);
          constrain(infer(context, level, term._0), {
                TAG: "SFunction",
                _0: infer(context, level, term._1),
                _1: res
              });
          return res;
      case "Lit" :
          if (term._0.TAG === "LitInt") {
            return {
                    TAG: "SPrim",
                    _0: "PrimInt"
                  };
          } else {
            return {
                    TAG: "SPrim",
                    _0: "PrimBool"
                  };
          }
      case "Rcd" :
          return {
                  TAG: "SRecord",
                  _0: Core__Dict.mapValues(term._0, (function(context){
                      return function (__x) {
                        return infer(context, level, __x);
                      }
                      }(context)))
                };
      case "Sel" :
          var name = term._1;
          var res$1 = fresh_var(name, level);
          constrain(infer(context, level, term._0), {
                TAG: "SRecord",
                _0: Object.fromEntries([[
                        name,
                        res$1
                      ]])
              });
          return res$1;
      case "Let" :
          var v = term._0;
          var v_ty = fresh_var(v, level + 1 | 0);
          var e_ty = infer(extend(context, v, {
                    TAG: "Simple_ty",
                    _0: v_ty
                  }), level + 1 | 0, term._1);
          constrain(e_ty, v_ty);
          _term = term._2;
          _context = extend(context, v, {
                TAG: "Poly",
                _0: level,
                _1: v_ty
              });
          continue ;
      
    }
  };
}

var recursive = Hashtbl.create(undefined, 5);

function coalesce_ty(inflight, simple_ty, polarity) {
  switch (simple_ty.TAG) {
    case "STyVar" :
        var vs = simple_ty._1;
        var name = simple_ty._0;
        var pname = [
          name,
          polarity
        ];
        if (Core__Option.isSome(Core__List.find(inflight, (function (v) {
                      return Caml_obj.equal(v, pname);
                    })))) {
          Hashtbl.add(recursive, pname, undefined);
          return {
                  TAG: "TyVar",
                  _0: name
                };
        }
        var bounds;
        bounds = polarity === "Positive" ? vs.lower_bounds : vs.upper_bounds;
        var coalesced_bounds = Core__List.map(bounds, (function (__x) {
                return coalesce_ty({
                            hd: pname,
                            tl: inflight
                          }, __x, polarity);
              }));
        var conn;
        conn = polarity === "Positive" ? (function (a, b) {
              return {
                      TAG: "Union",
                      _0: a,
                      _1: b
                    };
            }) : (function (a, b) {
              return {
                      TAG: "Intersection",
                      _0: a,
                      _1: b
                    };
            });
        var res = Core__List.reduce(coalesced_bounds, {
              TAG: "TyVar",
              _0: name
            }, conn);
        if (Core__Option.isSome(Hashtbl.find_opt(recursive, pname))) {
          return {
                  TAG: "Recursive",
                  _0: name,
                  _1: res
                };
        } else {
          return res;
        }
    case "SPrim" :
        return {
                TAG: "Prim",
                _0: simple_ty._0
              };
    case "SFunction" :
        return {
                TAG: "Function",
                _0: coalesce_ty(inflight, simple_ty._0, neg(polarity)),
                _1: coalesce_ty(inflight, simple_ty._1, polarity)
              };
    case "SRecord" :
        return {
                TAG: "Record",
                _0: Core__Dict.mapValues(simple_ty._0, (function (__x) {
                        return coalesce_ty(inflight, __x, polarity);
                      }))
              };
    
  }
}

function coalesce_ty$1(__x) {
  return coalesce_ty(/* [] */0, __x, "Positive");
}

var $$var = fresh_var("a", 1);

var context = {
  TAG: "Context",
  _0: {
    hd: [
      "if",
      {
        TAG: "Poly",
        _0: 0,
        _1: {
          TAG: "SFunction",
          _0: {
            TAG: "SPrim",
            _0: "PrimBool"
          },
          _1: {
            TAG: "SFunction",
            _0: $$var,
            _1: {
              TAG: "SFunction",
              _0: $$var,
              _1: $$var
            }
          }
        }
      }
    ],
    tl: {
      hd: [
        "+",
        {
          TAG: "Simple_ty",
          _0: {
            TAG: "SFunction",
            _0: {
              TAG: "SPrim",
              _0: "PrimInt"
            },
            _1: {
              TAG: "SFunction",
              _0: {
                TAG: "SPrim",
                _0: "PrimInt"
              },
              _1: {
                TAG: "SPrim",
                _0: "PrimInt"
              }
            }
          }
        }
      ],
      tl: /* [] */0
    }
  }
};

function if_(c, t, e) {
  return {
          TAG: "App",
          _0: {
            TAG: "App",
            _0: {
              TAG: "App",
              _0: {
                TAG: "Ref",
                _0: "if"
              },
              _1: c
            },
            _1: t
          },
          _1: e
        };
}

function add(a, b) {
  return {
          TAG: "App",
          _0: {
            TAG: "App",
            _0: {
              TAG: "Ref",
              _0: "+"
            },
            _1: a
          },
          _1: b
        };
}

var somercd = {
  TAG: "Rcd",
  _0: Object.fromEntries([
        [
          "cond",
          {
            TAG: "Lit",
            _0: {
              TAG: "LitBool",
              _0: false
            }
          }
        ],
        [
          "value",
          {
            TAG: "Lit",
            _0: {
              TAG: "LitInt",
              _0: 42
            }
          }
        ],
        [
          "default",
          {
            TAG: "Lit",
            _0: {
              TAG: "LitInt",
              _0: 1
            }
          }
        ]
      ])
};

var program_1 = {
  TAG: "Lam",
  _0: "x",
  _1: {
    TAG: "Lam",
    _0: "y",
    _1: if_({
          TAG: "Sel",
          _0: {
            TAG: "Ref",
            _0: "x"
          },
          _1: "cond"
        }, {
          TAG: "App",
          _0: {
            TAG: "App",
            _0: {
              TAG: "Ref",
              _0: "+"
            },
            _1: {
              TAG: "Sel",
              _0: {
                TAG: "Ref",
                _0: "x"
              },
              _1: "value"
            }
          },
          _1: {
            TAG: "Sel",
            _0: {
              TAG: "Ref",
              _0: "y"
            },
            _1: "value"
          }
        }, {
          TAG: "App",
          _0: {
            TAG: "App",
            _0: {
              TAG: "Ref",
              _0: "f"
            },
            _1: {
              TAG: "Ref",
              _0: "y"
            }
          },
          _1: {
            TAG: "Ref",
            _0: "x"
          }
        })
  }
};

var program_2 = {
  TAG: "Ref",
  _0: "f"
};

var program = {
  TAG: "Let",
  _0: "f",
  _1: program_1,
  _2: program_2
};

function show_ty(ty) {
  if (typeof ty !== "object") {
    if (ty === "Top") {
      return "top";
    } else {
      return "bottom";
    }
  }
  switch (ty.TAG) {
    case "Prim" :
        if (ty._0 === "PrimInt") {
          return "int";
        } else {
          return "bool";
        }
    case "Intersection" :
        return "(" + show_ty(ty._0) + " & " + show_ty(ty._1) + ")";
    case "Union" :
        return "(" + show_ty(ty._0) + " | " + show_ty(ty._1) + ")";
    case "Function" :
        return "(" + show_ty(ty._0) + " -> " + show_ty(ty._1) + ")";
    case "Record" :
        return "{ " + Core__Array.reduce(Object.entries(ty._0).map(function (param) {
                        return param[0] + ": " + show_ty(param[1]) + "; ";
                      }), "", (function (a, b) {
                      return a + b;
                    })) + "}";
    case "Recursive" :
        return "(fix " + ty._0 + " in " + show_ty(ty._1) + ")";
    case "TyVar" :
        return ty._0;
    
  }
}

var ty = coalesce_ty$1(infer(context, 0, program));

console.log(show_ty(ty));

export {
  neg ,
  Impossible ,
  Name_not_found ,
  find ,
  extend ,
  Type_error ,
  err ,
  fresh_var ,
  get_level ,
  freshen_above ,
  instantiate_at ,
  extrude ,
  show_simple_ty ,
  constrain ,
  infer ,
  coalesce_ty$1 as coalesce_ty,
  context ,
  if_ ,
  add ,
  somercd ,
  program ,
  show_ty ,
  ty ,
}
/* seen Not a pure module */
