let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self:  super: rec {
          dlist = self.callPackage ./dlist-0.8.0.2.nix { };
          servant-reflex = self.callPackage ../../servant-reflex { };
        };
      };
    };
  };
  reflex-platform = import ./reflex-platform { inherit config; };
  ghcjs = reflex-platform.ghcjs;
  servant-reflex = ghcjs.callPackage ../../servant-reflex {
  };
  drv = ghcjs.callPackage ./client.nix {
    inherit servant-reflex;
    # common = common { compiler = ghcjs; };
  };
in
  if reflex-platform.nixpkgs.pkgs.lib.inNixShell then drv.env else drv