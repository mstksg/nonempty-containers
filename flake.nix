{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/6083863f4ad49c59a74ed5496081727e23ccd754";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, haskell-nix, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        haskell-compiler-name = "ghc8104";
        haskell-nix-pkgs = haskell-nix.legacyPackages.${system};
        pkgs = import nixpkgs { inherit system; };
        project = haskell-nix-pkgs.haskell-nix.stackProject' {
          src = ./.;
          compiler-nix-name = haskell-compiler-name;
        };
     in {
       # Built by `nix build .`
       defaultPackage = project.hsPkgs.nonempty-containers.components.exes.nonempty-containers;
       
       tools = { stack2cabal = "latest"; };
       # This is used by `nix develop .` to open a shell for use with
       devShell = project.shellFor {
         buildInputs = with pkgs; [
           cabal-install
           haskell.packages."${haskell-compiler-name}".haskell-language-server
         ];
       };
     }
   );
}

