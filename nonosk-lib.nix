{ mkDerivation, arithmoi, base, bytestring, constraints, containers
, equational-reasoning, ghc-prim, ghc-typelits-knownnat
, ghc-typelits-natnormalise, haskell-src-exts, haskell-src-meta
, lens, mtl, parallel, smallcheck, stdenv, tasty
, tasty-expected-failure, tasty-golden, tasty-html
, tasty-quickcheck, tasty-rerun, tasty-smallcheck, template-haskell
, type-nat-solver, utility-ht
}:
mkDerivation {
  pname = "nonosk-lib";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring constraints containers equational-reasoning
    ghc-prim ghc-typelits-knownnat ghc-typelits-natnormalise
    haskell-src-exts haskell-src-meta lens mtl parallel
    template-haskell type-nat-solver utility-ht
  ];
  executableHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    arithmoi base constraints containers smallcheck tasty
    tasty-expected-failure tasty-golden tasty-html tasty-quickcheck
    tasty-rerun tasty-smallcheck type-nat-solver utility-ht
  ];
  license = stdenv.lib.licenses.gpl3;
}
