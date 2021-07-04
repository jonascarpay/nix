{ pkgs, lib, ... }:
with lib;
let
  scripts = {
    vimdir = ''
      set -e

      [[ -n "$1" ]] && cd "$1"
      FILE=$(fzf --preview='cat {+}')
      vim "$FILE"
    '';
    nix-fancy-uninstall = ''
      nix-env -q | fzf | xargs -I{} nix-env -e {}
    '';
    zap = ''
      xprop | grep -oP "PID\(CARDINAL\) = \K\d+" | xargs kill -9
    '';
  };

in
{
  home.packages =
    map
      (name: pkgs.writeShellScriptBin name (getAttr name scripts))
      (builtins.attrNames scripts);
}
