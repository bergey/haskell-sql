{
  extras = hackage:
    { packages = {
        haskell-sql-benchmarks = ./haskell-sql-benchmarks.nix;
        preql = hackage.preql."0.3".revisions.default;
        stm = hackage.stm."2.5.0.0".revisions.default;
      }; };
  resolver = "lts-15.4";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }
