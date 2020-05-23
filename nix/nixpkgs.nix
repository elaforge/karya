let
  # https://github.com/NixOS/nixpkgs/commits/nixos-19.09
  # head on 2020-05-23
  commit = "2efedf8fc74e8056f81bd18899276b085becf6dc";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-19.09";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0bn4r1qivvmh6klnq2y2pz18m0kd93gna8vkm3mkkavmpphkhd4w";
  };
in import nixpkgs
