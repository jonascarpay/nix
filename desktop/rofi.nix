{ pkgs, inputs, ... }:
let
  rofi-web-search = pkgs.writeShellScriptBin "rofi-web-search" (builtins.readFile ./rofi/web-search.sh);
  rofi-directory = pkgs.writeShellScriptBin "rofi-directory" ''
    HISTORY=~/.local/share/rofi/rofi-directory-history
    DIR=$(frecently $HISTORY view | rofi -dmenu -i -matching fuzzy -p Directory)
    DIR_REAL="''${DIR/#\~/$HOME}"
    if [ -d $DIR_REAL ]; then
      frecently "$HISTORY" bump "$DIR"
      st -d "$DIR_REAL" fish
    fi
  '';
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
    rofi-directory
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
