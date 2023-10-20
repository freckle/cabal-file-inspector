{
  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    freckle.url = "github:freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgsArgs = { inherit system; config = { }; };
        nixpkgs-stable = import inputs.nixpkgs-stable nixpkgsArgs;
        nixpkgs-unstable = import inputs.nixpkgs-unstable nixpkgsArgs;
        freckle = inputs.freckle.packages.${system};
      in
      {
        devShells.default = nixpkgs-stable.mkShell {
          buildInputs = [
            nixpkgs-stable.openssl.dev
            nixpkgs-stable.brotli.dev
            nixpkgs-stable.zlib.dev
          ];
          nativeBuildInputs = [
            nixpkgs-stable.pkg-config
            nixpkgs-unstable.cabal-install
            nixpkgs-unstable.ghc
            nixpkgs-unstable.haskell-language-server
            nixpkgs-unstable.haskellPackages.weeder
            freckle.fourmolu-0-13-x
            nixpkgs-stable.hlint
          ];
        };
      }
    );
}
