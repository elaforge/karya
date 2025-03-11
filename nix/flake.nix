# I have nixpkgs-sys, solely so I can get the system libjack2, jack protocol is
# version specific, so I need to use the system version instead of the one from
# my pinned nixpkgs.
#
# First thing is want a devShell.
#
# It puts the whole repo into /nix/store, but I just need the nix files.
# I'm not usually building in nix, just making the dev shell.
#
# - Can't use deps because builtins.currentSystem is missing.
# - localPkgs.nix uses nixpkgs.stdenv.isDarwin, I think I have to pass system.
{
  # description = "karya";
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/e6377ff35544226392b49fa2cf05590f9f0c4b43";
  };
  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      nixpkgs-sys = nixpkgs;
      # nixpkgs-sys = if isCi then nixpkgs else import <nixpkgs> {};
      localPkgs = import ./localPkgs.nix { inherit nixpkgs nixpkgs-sys; };
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShell.aarch64-darwin = pkgs.mkShell {
        buildInputs = with localPkgs; [
          # faust
          # fltk
          # fltk libjack2 libsamplerate rubberband supercollider
        ] ++ [
          pkgs.libsndfile
          pkgs.flac.dev pkgs.flac # libflac 1.3.3
        ];
        inputsFrom = [];
      };
    };
}
