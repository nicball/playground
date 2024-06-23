{ mkDerivation, attoparsec, base, bytestring, filepath, lib
, optparse-applicative, process, text, unix, utf8-string
}:
mkDerivation {
  pname = "haxd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring filepath optparse-applicative process
    text unix utf8-string
  ];
  description = "haxd: simplistic hexdump processor and editor";
  license = lib.licenses.agpl3Only;
  mainProgram = "haxd";
}
