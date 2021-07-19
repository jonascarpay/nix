{ pkgs, lib, ... }:
with lib;
let
  fzf = "${pkgs.fzf}/bin/fzf";
  scripts = {
    vimdir = ''
      set -e

      [[ -n "$1" ]] && cd "$1"
      FILE=$(${fzf} --preview='cat {+}')
      vim "$FILE"
    '';
    nix-fancy-uninstall = ''
      nix-env -q | ${fzf} | xargs -I{} nix-env -e {}
    '';
    zap = ''
      xprop | grep -oP "PID\(CARDINAL\) = \K\d+" | xargs kill -9
    '';
    nix-bump = ''
      shopt -s globstar
      for f in **/*.nix; do
        printf "$f:\t"
        ${pkgs.update-nix-fetchgit}/bin/update-nix-fetchgit -v $f
      done
    '';
  };

in
{
  home.packages =
    map
      (name: pkgs.writeShellScriptBin name (getAttr name scripts))
      (builtins.attrNames scripts);
}
