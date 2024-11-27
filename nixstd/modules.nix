# module C := { A, B }:
#   let ... in
#   {
#     c = A.f B.x A.D.y;
#   }

let

  import-module-name = module-name: eval-module (import (./. + "/${module-name}.nix"));

  eval-module = module:
    let
      dep-names = builtins.functionArgs module;
      deps = builtins.mapAttrs (k: _: import-module-name k) dep-names;
    in
    module deps;

in

eval-module
