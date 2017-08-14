{ mkDerivation, base, Cabal, deepseq, QuickCheck, stdenv }:
mkDerivation {
  pname = "dlist";
  version = "0.7.1.2";
  sha256 = "10rp96rryij7d8gz5kv8ygc6chm1624ck5mbnqs2a3fkdzqj2b9k";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [ base Cabal QuickCheck ];
  homepage = "https://github.com/spl/dlist";
  description = "Difference lists";
  license = stdenv.lib.licenses.bsd3;
}
