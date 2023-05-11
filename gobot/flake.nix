{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.default =
      with nixpkgs.legacyPackages.x86_64-linux;
      rustPlatform.buildRustPackage rec {
        pname = "gobot";
        version = "0.1.0";
        src = ./.;
        buildInputs = [ openssl ];
        nativeBuildInputs = [ pkg-config ];
        cargoLock = {
          lockFile = ./Cargo.lock;
        };
      };

  };
}
