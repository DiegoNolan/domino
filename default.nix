{ mkDerivation, adjunctions, amazonka, amazonka-core, amazonka-s3
, amazonka-ses, base, bytestring, classy-prelude, conduit
, conduit-extra, filepath, JuicyPixels, lens, lucid, mtl
, postgresql-simple, resourcet, servant, servant-lucid
, servant-server, stdenv, vector, wai, warp, wreq
}:
mkDerivation {
  pname = "domino";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    adjunctions amazonka amazonka-core amazonka-s3 amazonka-ses base
    bytestring classy-prelude conduit conduit-extra filepath
    JuicyPixels lens lucid mtl postgresql-simple resourcet servant
    servant-lucid servant-server vector wai warp wreq
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
}
