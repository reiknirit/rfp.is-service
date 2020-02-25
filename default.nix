{ pkgs ? import ./packages.nix {}, mkDerivation, aeson, base, base64-bytestring, bcrypt, bloodhound
, bytestring, containers, directory, dotenv, fast-logger, filepath
, hedis, hip, hpack, hslogger, http-client, http-types, hworker
, monad-control, monad-logger, mtl, optparse-applicative
, parser-combinators, persistent, persistent-postgresql
, persistent-template, safe, sendgrid-v3, servant
, servant-multipart, servant-server, stdenv, text, time
, transformers, unordered-containers, utf8-string, uuid, wai
, wai-cors, wai-extra, warp
}:
let 
  gitignoreSrc = pkgs.fetchFromGitHub { 
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "7415c4feb127845553943a3856cbc5cb967ee5e0";
    sha256 = "sha256:1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in
  mkDerivation {
    pname = "rfp-service";
    version = "0.0.5.0";
    src = gitignoreSource ./.;
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
      aeson base bcrypt bloodhound bytestring containers directory dotenv
      fast-logger filepath hedis hip hslogger http-client http-types
      hworker monad-control monad-logger mtl optparse-applicative
      parser-combinators persistent persistent-postgresql
      persistent-template safe sendgrid-v3 servant servant-multipart
      servant-server text time transformers unordered-containers uuid wai
      wai-cors wai-extra warp
    ];
    libraryToolDepends = [ hpack ];
    executableHaskellDepends = [
      base base64-bytestring bytestring dotenv fast-logger hedis hslogger
      http-types hworker monad-logger mtl optparse-applicative persistent
      persistent-postgresql persistent-template text time utf8-string wai
      wai-cors wai-extra warp
    ];
    prePatch = "hpack";
    description = "RFPService service";
    license = stdenv.lib.licenses.mit;
  }
