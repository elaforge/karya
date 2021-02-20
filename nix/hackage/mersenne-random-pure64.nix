{ mkDerivation, base, random, stdenv, time }:
mkDerivation {
  pname = "mersenne-random-pure64";
  version = "0.2.2.0";
  sha256 = "ef1400ddc1ddafb0b98d4331a58bdbe3b5209a81579e17f85f180731a96d75d1";
  revision = "1";
  editedCabalFile = "0pxq0xz42x60993rl4pr4pia80jv1xcqh6njvjzfvn75018j4gw5";
  libraryHaskellDepends = [ base random time ];
  homepage = "http://code.haskell.org/~dons/code/mersenne-random-pure64/";
  description = "Generate high quality pseudorandom numbers purely using a Mersenne Twister";
  license = stdenv.lib.licenses.bsd3;
}
