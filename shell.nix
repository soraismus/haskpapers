let
  pkgs = import <nixpkgs> {};

  node = pkgs.nodejs-10_x;
  yarn = pkgs.yarn;

  easy-purescript-nix = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "993f63359b64db080061b274e4688e3b80c4f68e";
    sha256 = "18b7fmmxkg38y1av9kfgcv2rikdlji51ya5b9p7sy3aml2hprmi5";
  });

  inputs = easy-purescript-nix.inputs // { inherit node yarn; };

  buildInputs = builtins.attrValues inputs;

  block = inputs // {
    inputs = inputs;

    buildInputs = buildInputs;

    shell = pkgs.runCommand "easy-purescript-nix-shell" {
      buildInputs = buildInputs;
    } "";
  };
in
  block.shell
