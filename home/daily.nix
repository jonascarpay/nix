{ pkgs, ... }:
let
  daily = pkgs.writeShellScriptBin "daily-dir" ''
    set -e
    BASE="$HOME/Daily"
    TEMPLATE="$BASE/template"
    DATE=$(date --date="4 hours ago" +%F)
    DAILY_DIR="$BASE/$DATE"
    if [ ! -d "$DAILY_DIR" ]; then
      cp -r "$TEMPLATE" "$DAILY_DIR"
      direnv allow "$DAILY_DIR"
    fi
    echo "$DAILY_DIR"
  '';

in
{
  home.packages = [ daily ];
  programs.fish.shellAbbrs.d = "cd (daily-dir)";
}
