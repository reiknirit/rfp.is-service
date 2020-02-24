{ nixpkgs ? import ./packages.nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bloodhound, bytestring
      , containers, directory, dotenv, fast-logger, filepath, hip, hpack
      , hslogger, http-client, http-types, monad-control, monad-logger
      , mtl, parser-combinators, persistent, persistent-postgresql
      , persistent-template, safe, servant, servant-multipart
      , servant-server, stdenv, text, time, transformers
      , unordered-containers, uuid, wai, wai-cors, wai-extra, warp
      }:
      let
        gitignoreSrc = pkgs.fetchFromGitHub { 
        owner = "hercules-ci";
        repo = "gitignore.nix";
        # put the latest commit sha of gitignore Nix library here:
        rev = "7415c4feb127845553943a3856cbc5cb967ee5e0";
        # use what nix suggests in the mismatch message here:
        sha256 = "sha256:1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
        };
        inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
      in
        mkDerivation {
          pname = "rfp-service";
          version = "0.0.4.0";
          src = gitignoreSource ./.;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson base bloodhound bytestring containers directory dotenv
            fast-logger filepath hip hslogger http-client http-types
            monad-control monad-logger mtl parser-combinators persistent
            persistent-postgresql persistent-template safe servant
            servant-multipart servant-server text time transformers
            unordered-containers uuid wai wai-cors wai-extra warp
          ];
          libraryToolDepends = [ hpack ];
          executableHaskellDepends = [
            base dotenv fast-logger hslogger http-types monad-logger mtl
            persistent persistent-postgresql persistent-template safe wai
            wai-cors wai-extra warp
          ];
          prePatch = "hpack";
          description = "RFPService service";
          license = stdenv.lib.licenses.mit;
        };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
