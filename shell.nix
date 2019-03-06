{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, dhall, optparse-applicative, parsec
      , safe, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, time
      }:
      mkDerivation {
        pname = "gentodo";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base dhall optparse-applicative parsec safe text time
        ];
        executableHaskellDepends = [ base text time ];
        testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
