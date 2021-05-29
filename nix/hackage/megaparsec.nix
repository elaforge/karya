{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, mtl, parser-combinators, scientific, stdenv
, text, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "9.0.1";
  sha256 = "7228bc49d8636632b481eb13f16f2a9633007b8f55ebc0105f517ad7f71f2501";
  revision = "1";
  editedCabalFile = "00vjc5b1x6yd0jqsbcahvghlkwai65dl1ib6744a0lhsa9vsni12";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq text weigh
  ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
