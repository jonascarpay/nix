{ pkgs, ... }:
let
  mkSearch = name: url: pkgs.writeShellScriptBin "rofi-${name}-search" ''
    HISTORY=~/.local/share/rofi/rofi-${name}-search-hisory
    touch $HISTORY
    QUERY=$(tac $HISTORY | rofi -dmenu -i -matching fuzzy -p "${name} search")

    if [[ -n $QUERY ]]; then
      URL="${url}$QUERY"
      xdg-open "$URL"
      sed --quiet "/$QUERY/!p" -i $HISTORY
      echo "$QUERY" >>$HISTORY
      head -n -100 $HISTORY
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
    (mkSearch "duckduckgo" "https://www.duckduckgo.com/?q=")
    (mkSearch "google" "https://www.google.com/search?q=")
    (mkSearch "hoogle-local" "http://localhost:8080/?hoogle=")
    (mkSearch "hoogle" "https://hoogle.haskell.org/?hoogle=")
    (mkSearch "hackage" "https://hackage.haskell.org/packages/search?terms=")
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
