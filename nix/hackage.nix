# Produce a { name = src; } for each package described in hackage.json,
# which is produced by make_hackage.py.
{ nixpkgs }:
let
  # Combine the hackage source and the updated cabal file from the
  # all-cabal-files repo, which has had metadata revisions applied.
  mkSrc = { pkg, ver, cabal, cabalHash, src, srcHash }:
    nixpkgs.stdenv.mkDerivation {
      name = "${pkg}-${ver}-source";
      PATH = "${nixpkgs.coreutils}/bin";
      src = builtins.fetchTarball {
        url = src;
        sha256 = srcHash;
      };
      cabal = builtins.fetchurl {
        url = cabal;
        sha256 = cabalHash;
      };
      inherit pkg;
      builder = "${nixpkgs.bash}/bin/bash";
      args = ["-euo" "pipefail" (builtins.toFile "builder.sh" ''
        mkdir $out
        cd $out
        for f in $src/*; do
          ln -s "$f"
        done
        rm $pkg.cabal
        cp $cabal $pkg.cabal
        # Some packages (language-c at least) checked 'dist' into source
        # control, and then I get an unwritable 'dist' in the build dir.
        rm -f dist
      '')];
    };
  compose = nixpkgs.lib.foldr (f: g: x: f (g x)) (x: x);
in compose [
  (builtins.mapAttrs (_: mkSrc))
  builtins.listToAttrs
  (map (a: { name = a.pkg; value = a; }))
  builtins.fromJSON
  builtins.readFile
] ./hackage.json
