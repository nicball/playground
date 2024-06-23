{
  description = "haxd: simplistic hexdump processor and editor";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.haxd = nixpkgs.legacyPackages.x86_64-linux.haskellPackages.callPackage ./default.nix {};

    packages.x86_64-linux.default = self.packages.x86_64-linux.haxd;

  };
}
