{ mkDerivation, array, base, bytestring, containers, directory
, dlist, filepath, HUnit, language-c, pretty, process, shelly
, lib, test-framework, test-framework-hunit, text, transformers
}:
mkDerivation {
  pname = "c2hs";
  version = "0.28.7";
  sha256 = "e1146a80006b24bede1947612bdbd81da1285a7fa48370aad5799e619d576745";
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    array base bytestring containers directory dlist filepath
    language-c pretty process
  ];
  testHaskellDepends = [
    base filepath HUnit shelly test-framework test-framework-hunit text
    transformers
  ];
  homepage = "https://github.com/haskell/c2hs";
  description = "C->Haskell FFI tool that gives some cross-language type safety";
  license = lib.licenses.gpl2;
}
