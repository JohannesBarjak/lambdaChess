{
  description = "A pure chess program";

  outputs = { self, nixpkgs ? import <nixpkgs> }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    pkgName = "lambdaChess";
    haskPkgs = pkgs.haskell.packages.ghc982;

  in {
    packages.${system}.${pkgName} = haskPkgs.developPackage { root  = ./.; };
    defaultPackage.${system} = self.packages.${system}.${pkgName};

    devShells = {
      ${system}.default = pkgs.mkShell {
        buildInputs = [
          ( haskPkgs.ghcWithPackages ( p: [
            p.haskell-language-server
            p.ghcid
            p.cabal-install
          ]))
        ];
      };
    };
  };
}
