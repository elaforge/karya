# Read a json file produced by make_hackage.py with package versions.
# TODO either port it to nix, or run the script as a derivation
{ ghcVersion, profiling }:
let
  nixpkgs = import ./nixpkgs.nix { inherit config; };
  inherit (nixpkgs) lib;

  config = {
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${ghcVersion}" = pkgs.haskell.packages."${ghcVersion}".override {
            inherit all-cabal-hashes;
          };
        };
      };
    };
  };
  all-cabal-hashes =
    let commit = "21fa0f534f906ead4abe15b071564e21e916c9f7";
    in nixpkgs.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/"
        + "${commit}.tar.gz";
      sha256 = "0niyw8l5qbnpwdqbaynyzsp85p7bv42rha9b8gjgjarg865r8iyy";
    };
  ghc = nixpkgs.haskell.packages."${ghcVersion}";

  # Fix broken stuff in hackage that they just never fix.
  callHackageArgs = {
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

  importPackage = ghc: name: fn:
    let args = callHackageArgs."${name}" or {};
    in overrideCabal (ghc.callPackage fn args);

  callHackage = name: ver:
    let args = callHackageArgs."${name}" or {};
    in overrideCabal (ghc.callHackage name ver args);

  # Turn off the auto-SCCs in hackage libraries.
  disableAutoProf = drv: nixpkgs.haskell.lib.overrideCabal drv
    (drv: { profilingDetail = "none"; });

  overrideCabal = with nixpkgs.haskell.lib; compose [
    (if profiling then (x: x) else disableLibraryProfiling)
    disableAutoProf
    dontCheck
    dontBenchmark
    dontCoverage
  ];
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

in rec {
  packageVersions = parseFreezeFile ../doc/cabal/all-deps.cabal.config;
  fromNixpkgs = builtins.mapAttrs callHackage
    (lib.filterAttrs (k: _: k == "ansi-wl-pprint") packageVersions);
    # TODO: this should be just the new packages

  packages = builtins.mapAttrs
    (importPackage nixpkgs.haskell.packages."${ghcVersion}") packageNames;

  nixFiles = nixpkgs.stdenv.mkDerivation {
    name = "nixFiles";
    phases = "buildPhase";
    # This is really stupid but I can't think of any other way to zip
    # in shell.
    cmds = unlines (map
      (drv: "cp ${drv.cabal2nixDeriver}/default.nix $out/${drv.pname}.nix")
        (builtins.attrValues fromNixpkgs)
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
      name = lib.removeSuffix ".nix" fn;
      value = ./hackage + "/${fn}";
    };
    in builtins.listToAttrs
      (map mk (builtins.attrNames (builtins.readDir ./hackage)));

  overrides = ghc: builtins.mapAttrs (importPackage ghc) packageNames;

  # versions mismatch, causes nix to segfault
  segfault = callHackage nixpkgs.haskell.packages.ghc882 "ghc" "8.8.3";
}
