let
  my-poison = v22-11; # pick your poison

  # https://channels.nixos.org/
  # Navigate to a subdir, get git-revision
  # Enter a bogus sha256, nix-instantiate --attr buildEnv, fix sha

  v21-11 = {
    version = "21.11";
    commit = "e6377ff35544226392b49fa2cf05590f9f0c4b43";
    sha256 = "1fra9wwy5gvj5ibayqkzqpwdf715bggc0qbmrfch4fghwvl5m70l";
  };
  v22-11 = {
    version = "22.11";
    commit = "4d2b37a84fad1091b9de401eb450aae66f1a741e";
    sha256 = "sha256:11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
  };
  v23-11 = {
    version = "23.11";
    commit = "057f9aecfb71c4437d2b27d3323df7f93c010b7e";
    sha256 = "sha256:1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
  };

  fetch = ver: builtins.fetchTarball {
    name = "nixpkgs-${ver.version}";
    url = "https://github.com/nixos/nixpkgs/archive/${ver.commit}.tar.gz";
    inherit (ver) sha256;
  };

in import (fetch my-poison)
