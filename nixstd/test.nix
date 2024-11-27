{ Contract , Either, Parsec, Applicative, Function }:

with Contract;

let
  f = function-c int-c float-c (i: 5.0);
  config-c = attrs-c (k: { name = string-c; enable = bool-c; }.${k} or none-c);
  a = config-c {
    name = "haha";
    enable = false;
  };
  should-fail = config-c {
    name = 123;
    enable = true;
  };
  also-fail = config-c {
    name = "hoho";
    enable = false;
    heihei = 42;
  };
  ws = with Parsec; optional (many space);
  numlist = with Parsec; sep-by (Applicative.right applicative ws (exactly ",")) (Applicative.right applicative ws number);
in

Parsec.run-parsec "1, 2 , 34" (Applicative.left Parsec.applicative numlist Parsec.eof)
