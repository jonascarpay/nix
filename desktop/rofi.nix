{ pkgs, inputs, ... }:
let
  rofi-web-search = pkgs.writeShellScriptBin "rofi-web-search" (builtins.readFile ./rofi/web-search.sh);

  rofi-directory =
    let
      script = pkgs.writeShellScriptBin "rofi-directory" ''
        HISTORY=~/.local/share/rofi/rofi-directory-history
        DIR=$(frecently $HISTORY view | sed "s#$HOME#~#" | rofi -dmenu -i -matching fuzzy -p Directory)
        DIR_REAL=$(realpath "''${DIR/#\~/$HOME}")
        if [ -d $DIR_REAL ]; then
          frecently "$HISTORY" bump "$DIR_REAL"
          st -d "$DIR_REAL" fish
        fi
      '';
    in
    {
      home.packages = [ script ];
      programs.fish.shellInit = ''
        function __rofi-directory-hook --on-variable PWD --description 'add current directory to directory history'
          frecently $HOME/.local/share/rofi/rofi-directory-history bump "$PWD"
        end
      '';
    };
in
{
  imports = [ rofi-directory ];
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
    theme =
      let
        theme-source = pkgs.fetchFromGitHub {
          owner = "lr-tech";
          repo = "rofi-themes-collection";
          rev = "5ae9b23ef58893229b0df57ad750ad84801a632e";
          sha256 = "sha256-ecCQcDVWXpSilER99OROW9wutIq58llUGjFTn9rH2RM=";
        };
      in
      "${theme-source}/themes/nord.rasi"
    ;
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
