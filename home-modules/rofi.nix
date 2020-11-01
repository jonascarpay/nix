{ pkgs, config, ... }:
let
  upkgs = import <unstable> {};
in
{
  programs.rofi = {
    enable = true;
    theme = "~/.cache/wal/colors-rofi-dark.rasi";
    # FIXME package = upkgs.rofi.override { plugins = [ upkgs.rofi-calc ]; };
    # fullscreen = true;
    # terminal = "";
    font = "SauceCodePro Nerd Font Mono 24";
    lines = 5;
    extraConfig = ''
      // rofi.modi: combi,calc
      rofi.modi: combi
      rofi.combi-modi run,window
      rofi.matching: fuzzy
    '';
  };

}
