{ mkDerivation, aeson, base, containers, ghcjs-base, ghcjs-dom
, lens, lucid, reflex, reflex-dom, safe, servant, servant-lucid
, servant-reflex, stdenv, text
}:
mkDerivation {
  pname = "ghcjs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers ghcjs-base ghcjs-dom lens lucid reflex
    reflex-dom safe servant servant-lucid servant-reflex text
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/ghcjs#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
