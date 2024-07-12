{
  description = "Nicball's trash programs";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.wxd =
      with nixpkgs.legacyPackages.x86_64-linux;
      stdenv.mkDerivation {
        pname = "wxd";
        version = "0.1.0";
        src = ./wxd;
        nativeBuildInputs = [ clang ];
        buildPhase = "mkdir target; make target/wxd_intrin";
        installPhase = "mkdir -p $out/bin; cp target/wxd_intrin $out/bin/wxd";
      };

  };
}
