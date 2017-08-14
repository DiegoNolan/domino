let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          domino =
            haskellPackagesNew.callPackage ./default.nix { };

          snap = pkgs.haskell.lib.dontCheck haskellPackagesOld.snap;

          snaplet-postgresql-simple = pkgs.haskell.lib.dontCheck
            (haskellPackagesNew.callPackage ../snaplet-postgresql-simple { });

          servant-snap =
            pkgs.haskell.lib.dontCheck haskellPackagesOld.servant-snap;
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { domino = pkgs.haskellPackages.domino;
  }
