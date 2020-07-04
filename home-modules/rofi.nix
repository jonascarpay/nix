{ pkgs, config, ... }:
let
  upkgs = import <unstable> {};
in {
  programs.rofi = {
    enable = true;
    theme = "~/.cache/wal/colors-rofi-dark.rasi";
    package = upkgs.rofi.override {
      plugins = [ upkgs.rofi-calc ];
    };
    extraConfig = ''
      rofi.modi: combi,calc,keys
      rofi.combi-modi run,window,calc
      rofi.matching: fuzzy
    '';
  };

}
