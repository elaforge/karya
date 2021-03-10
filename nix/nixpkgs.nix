let
  # https://github.com/NixOS/nixpkgs/commits/nixos-19.09
  # head on 2020-05-23
  v19-09 = {
    version = "19.09";
    commit = "2efedf8fc74e8056f81bd18899276b085becf6dc";
    sha256 = "0bn4r1qivvmh6klnq2y2pz18m0kd93gna8vkm3mkkavmpphkhd4w";
  };
# https://releases.nixos.org/nixos/20.09/nixos-20.09.3124.2118cf551b9/git-revision
  v20-09 = {
    version = "20.09";
    commit = "2118cf551b9944cfdb929b8ea03556f097dd0381";
    sha256 = "0ajsxh1clbf3q643gi8v6b0i0nn358hak0f265j7c1lrsbxyw457";
  };

  fetch = ver: builtins.fetchTarball {
    name = "nixpkgs-${ver.version}";
    url = "https://github.com/nixos/nixpkgs/archive/${ver.commit}.tar.gz";
    inherit (ver) sha256;
  };

# in import (fetch v19-09)
in import (fetch v20-09)
