{ pkgs }: {
  deps = [
    pkgs.haskellPackages.time_1_12_2
    pkgs.haskellPackages.ghc
    pkgs.haskell-language-server
    pkgs.cabal-install
  ];
}