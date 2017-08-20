{ mkDerivation, base, http-media, lucid, servant, stdenv }:
mkDerivation {
  pname = "servant-lucid";
  version = "0.7.1";
  sha256 = "0h7yw140ymigrzrzp2vkkhg13gg1d8pj9xmcpq8bw2cv2myvl9pc";
  revision = "5";
  editedCabalFile = "0hqwbh0mcl3mdv0aj9xvnzpqdv8am07i48cjpx96yihkg86r5phm";
  libraryHaskellDepends = [ base http-media lucid servant ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Servant support for lucid";
  license = stdenv.lib.licenses.bsd3;
}
