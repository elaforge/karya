let
  my-poison = v21-11; # pick your poison

  # https://releases.nixos.org/nixos/20.09/nixos-20.09.3124.2118cf551b9/git-revision
  v20-09 = {
    version = "20.09";
    commit = "2118cf551b9944cfdb929b8ea03556f097dd0381";
    sha256 = "0ajsxh1clbf3q643gi8v6b0i0nn358hak0f265j7c1lrsbxyw457";
  };
  v21-11 = {
    version = "21.11";
    commit = "e6377ff35544226392b49fa2cf05590f9f0c4b43";
    sha256 = "1fra9wwy5gvj5ibayqkzqpwdf715bggc0qbmrfch4fghwvl5m70l";
  };
  unstable = {
    version = "unstable";
    commit = "e83b3f3394834c41c0d25017f6808d65c3d6f880";
    sha256 = "0wbkyz460547x58mrzvq52qpni38c6fcsjc8mb7v0311p8kyx8dd";
  };

  fetch = ver: builtins.fetchTarball {
    name = "nixpkgs-${ver.version}";
    url = "https://github.com/nixos/nixpkgs/archive/${ver.commit}.tar.gz";
    inherit (ver) sha256;
  };

in import (fetch my-poison)
