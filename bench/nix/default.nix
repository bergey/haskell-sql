let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };

    haskell-nix-src =
      let snapshot = builtins.fromJSON (builtins.readFile ./iohk-haskell-nix.json);
      in (import nixpkgs {}).fetchgit {
        name = "haskell-lib";
        inherit (snapshot) url rev sha256 fetchSubmodules;
      };

    hackageSrc = builtins.fetchTarball https://github.com/input-output-hk/hackage.nix/archive/master.tar.gz;


    pkgs = import nixpkgs (import haskell-nix-src {
      # https://input-output-hk.github.io/haskell.nix/tutorials/hackage-stackage/
      sourcesOverride = {
        hackage = hackageSrc;
        # stackage = stackageSrc
      };
    }).nixpkgsArgs;

    pkgSet = pkgs.haskell-nix.mkStackPkgSet {
      stack-pkgs = import ./pkgs.nix;
      pkg-def-extras = [];
      modules = [];
    };

in
  pkgSet.config.hsPkgs
