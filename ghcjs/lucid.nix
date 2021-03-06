{ mkDerivation, base, bifunctors, blaze-builder, bytestring
, containers, criterion, deepseq, hashable, hspec, HUnit, mmorph
, mtl, parsec, stdenv, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "lucid";
  version = "2.9.8.1";
  sha256 = "026s82bh3a4lgmpy9445i5f6q0iiqpc8cxxx8rhmn32nrqhf187b";
  libraryHaskellDepends = [
    base blaze-builder bytestring containers hashable mmorph mtl text
    transformers unordered-containers
  ];
  testHaskellDepends = [
    base bifunctors hspec HUnit mtl parsec text
  ];
  benchmarkHaskellDepends = [
    base blaze-builder bytestring criterion deepseq text transformers
  ];
  homepage = "https://github.com/chrisdone/lucid";
  description = "Clear to write, read and edit DSL for HTML";
  license = stdenv.lib.licenses.bsd3;
}
