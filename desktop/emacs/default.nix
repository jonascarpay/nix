{ pkgs, inputs, ... }:
{
  imports = [
    ./dmenu.nix
    inputs.nix-doom-emacs.hmModule
  ];
  home.packages = [
    pkgs.ripgrep
    pkgs.emacs-all-the-icons-fonts
    pkgs.sqlite
  ];
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./doom.d;
  };
  programs.git.enable = true;
  programs.git.ignores = [
    "**/#*#"
    "*.elc"
    "*.rel"
    "*/ltximg/"
    "*_archive"
    "*_flymake.*"
    ".#*"
    ".cask/"
    ".dir-locals.el"
    ".org-id-locations"
    "/.emacs.desktop"
    "/.emacs.desktop.lock"
    "/auto/"
    "/elpa/"
    "/elpa/"
    "/eshell/*"
    "/network-security.data"
    # "/server/"
    "auto-save-list"
    "dist/"
    "flycheck_*.el"
    "tramp"
  ];
}
