{}:

rec {

  id = x: x;

  const = c: _: c;

  compose = f: g: x: f (g x);

  composes = fs: builtins.foldl' compose id fs;

  fix = f: f (fix f);

}
