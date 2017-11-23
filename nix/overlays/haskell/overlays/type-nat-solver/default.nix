nixpkgs: self: super: {
  type-nat-solver = self.callPackage ./type-nat-solver.nix {
    # There's a haskell package called z3 that would automatically be
    # chosen by callPackage; we need the z3 executable
    z3-exe = nixpkgs.z3;
  };
}
