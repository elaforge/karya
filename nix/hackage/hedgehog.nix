{ mkDerivation, ansi-terminal, async, barbies, base, bytestring
, concurrent-output, containers, deepseq, directory, erf
, exceptions, lifted-async, mmorph, monad-control, mtl, pretty-show
, primitive, random, resourcet, stdenv, stm, template-haskell, text
, time, transformers, transformers-base, wl-pprint-annotated
, lib
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.1.1";
  sha256 = "ded621edfef36d2bef2ecce10f331f3327af322de2e30c7bf9fbb3d7985ca7e3";
  libraryHaskellDepends = [
    ansi-terminal async barbies base bytestring concurrent-output
    containers deepseq directory erf exceptions lifted-async mmorph
    monad-control mtl pretty-show primitive random resourcet stm
    template-haskell text time transformers transformers-base
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show text transformers
  ];
  doCheck = false;
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
}
