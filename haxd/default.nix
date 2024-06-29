{ mkDerivation, attoparsec, base, bytestring, filepath, lib
, optparse-applicative, process, text, unix
}:
mkDerivation {
  pname = "haxd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring filepath optparse-applicative process
    text unix
  ];
  description = "haxd: simplistic hexdump processor and editor";
  license = lib.licenses.agpl3Only;
  mainProgram = "haxd";
}
