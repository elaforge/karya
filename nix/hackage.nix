# Read a json file produced by make_hackage.py with package versions.
# TODO either port it to nix, or run the script as a derivation
{ nixpkgs, ghc, profiling }:
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

  unlines = builtins.concatStringsSep "\n";
  lines = compose [
    (builtins.filter (s: builtins.isString s && s != ""))
    (builtins.split "\n")
    builtins.readFile
  ];

  compose = nixpkgs.lib.foldr (f: g: x: f (g x)) (x: x);

  importPackage = ghc: name: fn:
    let args = (callHackageArgs ghc)."${name}" or {};
    in overrideCabal (ghc.callPackage fn args);

in rec {
  packages = parseFreezeFile ../doc/cabal/all-deps.cabal.config;

  nixFiles = nixpkgs.stdenv.mkDerivation {
    name = "nixFiles";
    phases = "buildPhase";
    # This is really stupid but I can't think of any other way to zip
    # in shell.
    cmds = unlines (map
      (drv: "cp ${drv.cabal2nixDeriver}/default.nix $out/${drv.pname}.nix")
        (builtins.attrValues (overrides ghc))
    );
    buildPhase = ''
      mkdir $out
      (
      set -eux
      IFS=$'\n'
      for cmd in $cmds; do
        IFS=' '
        eval $cmd
      done
      )
    '';
  };

  packageNames =
    let mk = fn: {
      name = nixpkgs.stdenv.lib.removeSuffix ".nix" fn;
      value = ./hackage + "/${fn}";
    };
    in builtins.listToAttrs
      (map mk (builtins.attrNames (builtins.readDir ./hackage)));

  overrides = ghc: builtins.mapAttrs (importPackage ghc) packageNames;
  # overrides = ghc: builtins.mapAttrs (callHackage ghc) packages;

  # versions mismatch, causes nix to segfault
  segfault = callHackage nixpkgs.haskell.packages.ghc882 "ghc" "8.8.3";

  all-cabal-hashes =
    let commit = "21fa0f534f906ead4abe15b071564e21e916c9f7";
    in nixpkgs.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/"
        + "${commit}.tar.gz";
      sha256 = "0niyw8l5qbnpwdqbaynyzsp85p7bv42rha9b8gjgjarg865r8iyy";
    };
}
