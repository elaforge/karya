{ mkDerivation, base, lib, transformers }:
mkDerivation {
  pname = "unliftio-core";
  version = "0.2.0.1";
  sha256 = "919f0d1297ea2f5373118553c1df2a9405d8b9e31a8307e829da67d4953c299a";
  revision = "1";
  editedCabalFile = "16k5fxlm9xpbd0ca861nmhb1j2ahyid02m1vbg1vzb5ckbm48glv";
  libraryHaskellDepends = [ base transformers ];
  homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO";
  license = lib.licenses.mit;
}
