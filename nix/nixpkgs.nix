# /nix/store/cy3b2hsy2dk9cb17dvkn4ninca7ffrl1-nixpkgs-19.09
let
  commit = "4ad6f1404a8cd69a11f16edba09cc569e5012e42";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-19.09";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1pclh0hvma66g3yxrrh9rlzpscqk5ylypnmiczz1bwwrl8n21q3h";
  };
in import nixpkgs
