{ pkgs, inputs, ... }:
let
  rofi-web-search = pkgs.writeShellScriptBin "rofi-web-search" (builtins.readFile ./rofi/web-search.sh);
in
{
  home.packages = [
    pkgs.rofi-power-menu
    pkgs.rofi-systemd
    pkgs.rofimoji
    # pkgs.rofi-vpn
    # pkgs.rofi-mpd
    pkgs.rofi-pass
    rofi-web-search
    inputs.frecently.defaultPackage.${pkgs.system}
  ];
  programs.rofi = {
    enable = true;
    theme = "Pop-Dark";
    pass = {
      enable = true;
      stores = [ "~/Passwords/" ];
    };
    plugins = [
      pkgs.rofi-calc
    ];
    font = "SauceCodePro Nerd Font 18";
  };
}
