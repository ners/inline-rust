{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    language-rust = {
      url = "github:GaloisInc/language-rust";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" ])
          root;
      };
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp':
          let
            hp = tryEval hp';
            version = getVersion hp.value.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
          in
          if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
          then acc // { ${ghcName} = hp.value; }
          else acc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      pname = "inline-rust";
      pnames = [ pname ];
      haskell-overlay = final: prev: hfinal: hprev: with prev.haskell.lib.compose; {
        language-rust = hfinal.callCabal2nix "language-rust" inputs.language-rust { };
        ${pname} = addBuildDepend prev.cargo (hfinal.callCabal2nix pname (sourceFilter ./.) { });
      };
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeManyExtensions [
            prev.haskell.packageOverrides
            (haskell-overlay final prev)
          ];
        };
      };
    in
    {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
          libs = pkgs.buildEnv {
            name = "${pname}-libs";
            paths =
              lib.mapCartesianProduct
                ({ hp, pname }: hp.${pname})
                { hp = attrValues hps; pname = pnames; };
            pathsToLink = [ "/lib" ];
          };
          docs = pkgs.buildEnv {
            name = "${pname}-docs";
            paths = map (pname: pkgs.haskell.lib.documentationTarball hps.default.${pname}) pnames;
          };
          sdist = pkgs.buildEnv {
            name = "${pname}-sdist";
            paths = map (pname: pkgs.haskell.lib.sdistTarball hps.default.${pname}) pnames;
          };
          docsAndSdist = pkgs.linkFarm "${pname}-docsAndSdist" { inherit docs sdist; };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.symlinkJoin {
            name = "${pname}-all";
            paths = [ libs docsAndSdist ];
            inherit (hps.default.syntax) meta;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = with pkgs'; with haskellPackages; [
                  cabal-gild
                  cabal-install
                  cargo
                  fourmolu
                  rustc
                ] ++ lib.optionals (lib.versionAtLeast (lib.getVersion hp.ghc) "9.4") [
                  hp.haskell-language-server
                ];
              };
            });
        }
      );
}
