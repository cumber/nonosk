self: super:
  let extendAll = (exts: hpkgs:
        super.lib.foldl' (hpkgs: ext: hpkgs.extend (ext super)) hpkgs exts
      );
      extendWithOverlays = extendAll (self.lib.importDir ./overlays);
      extendIfHpkgs = (_: set:
        if builtins.hasAttr "ghc" set
          then extendWithOverlays set
          else set
      );
   in {
        haskell = super.haskell // {
          packages = super.lib.mapAttrs extendIfHpkgs super.haskell.packages;
        };
        haskellPackages = self.haskell.packages.ghc802 ;
      }
