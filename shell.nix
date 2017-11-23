{ nixpkgsPath ? <nixpkgs>, compiler ? "ghc802" }:
  let nixpkgs = import nixpkgsPath {
        overlays = import ./nix/overlays.nix;
      };
   in (import ./default.nix { inherit nixpkgs compiler; }).env
