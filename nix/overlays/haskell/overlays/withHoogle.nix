nixpkgs: self: super:
  if true # shouldHoogle
    then {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    }
    else super
