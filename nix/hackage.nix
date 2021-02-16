# Read a json file produced by make_hackage.py with package versions.
# TODO either port it to nix, or run the script as a derivation
{ nixpkgs, profiling }:
let
  # Fix broken stuff in hackage that they just never fix.
  callHackageArgs = ghc: {
    # Otherwise zlib is circular because nixpkgs doesn't differentiate
    # haskell and C deps.
    zlib = { inherit (nixpkgs) zlib; };
    # Apparently missing this dep.
    digest = { inherit (nixpkgs) zlib; };
    hlibgit2 = {
      mkDerivation = args: ghc.mkDerivation (args // {
        # New gcc doesn't like -Wno-format without -Wno-format-security.
        patches = [./hlibgit2.patch];
      });
    };
  };

  callHackage = ghc: name: ver:
    let args = (callHackageArgs ghc)."${name}" or {};
    in overrideCabal (ghc.callHackage name ver args);

  overrideCabal = drv:
    with nixpkgs.haskell.lib;
      (if profiling then (x: x) else disableLibraryProfiling)
        (dontCheck (dontBenchmark (dontCoverage drv)));
    # This should work better than jailbreak-cabal, but apparently needs
    # brand-new Cabal (3.0.1.0 lacks it, 3.2.0.0 has it).
    # jailbreak = drv: nixpkgs.haskell.lib.appendConfigureFlags drv
    #   ["--allow-older" "--allow-newer"];

  parseFreezeFile = compose [
    builtins.listToAttrs
    (map parseLine)
    (builtins.filter (line: line != "constraints:"))
    lines
  ];

  # File must have been processed with tools/freeze_fix.py, which puts
  # "constraints:" on its own line.
  parseLine = line:
    let m = builtins.match " *([^ ]+) ==([0-9.]+),?" line;
    in if builtins.isList m && builtins.length m == 2
      then { name = builtins.elemAt m 0; value = builtins.elemAt m 1; }
      else abort "can't parse line: ${line}";

  lines = compose [
    (builtins.filter (s: builtins.isString s && s != ""))
    (builtins.split "\n")
    builtins.readFile
  ];

  compose = nixpkgs.lib.foldr (f: g: x: f (g x)) (x: x);

in rec {
  packages = parseFreezeFile ../doc/cabal/all-deps.cabal.config;

  # versions mismatch, causes nix to segfault
  segfault = callHackage nixpkgs.haskell.packages.ghc882 "ghc" "8.8.3";

  overrides = ghc: builtins.mapAttrs (callHackage ghc) packages;

  all-cabal-hashes =
    let commit = "21fa0f534f906ead4abe15b071564e21e916c9f7";
    in nixpkgs.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/"
        + "${commit}.tar.gz";
      sha256 = "0niyw8l5qbnpwdqbaynyzsp85p7bv42rha9b8gjgjarg865r8iyy";
    };
}
