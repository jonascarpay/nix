{ pkgs, inputs, ... }:
let

  frecently = inputs.frecently.defaultPackage.${pkgs.system};

  rofi-web-search = pkgs.writeShellScriptBin "rofi-web-search" (builtins.readFile ./rofi/web-search.sh);

  rofi-directory = let history = "~/.local/share/rofi/rofi-directory-history"; in
    {
      systemd.user.timers.directory-frecency-cleanup = {
        Unit.Description = "Remove missing directories from directory frecency";
        Install.WantedBy = [ "timers.target" ];
        Timer.OnCalendar = "*:0/5";
      };
      systemd.user.services.directory-frecency-cleanup = {
        Unit.Description = "Remove missing directories from directory frecency";
        Service.Type = "oneshot";
        Service.ExecStart =
          let
            # writeShellApplication allows passing an environment, which we need, because in systemd you don't inherit it
            script = pkgs.writeShellApplication {
              name = "directory-cleanup";
              runtimeInputs = [ frecently pkgs.coreutils ];
              text = ''
                for dir in $(frecently ${history} view); do
                  if [ ! -d "$dir" ]; then
                    echo "Removing $dir"
                    frecently ${history} delete "$dir"
                  fi
                done
              '';
            };
          in
          "${script}/bin/directory-cleanup";
      };
      home.packages =
        let script =
          pkgs.writeShellScriptBin "rofi-directory" ''
            set -e
            DIR=$(frecently ${history} view | sed "s#$HOME#~#" | rofi -dmenu -i -matching fuzzy -p Directory)
            DIR_REAL=$(realpath "''${DIR/#\~/$HOME}")
            if [ -d $DIR_REAL ]; then
              frecently ${history} bump "$DIR_REAL"
              st -d "$DIR_REAL" fish
            fi
          '';
        in [ script ];
      programs.fish.shellInit = ''
        function __rofi-directory-hook --on-variable PWD --description 'add current directory to directory history'
          frecently $HOME/.local/share/rofi/rofi-directory-history bump "$PWD"
        end
      '';
    };

  rofi-command =
    let
      script = pkgs.writeShellScriptBin "rofi-command" ''
        set -e
        HISTORY=~/.local/share/rofi/command-frecency
        CMD=$(frecently $HISTORY view | rofi -dmenu -i -matching fuzzy -p Command)
        frecently "$HISTORY" bump "$CMD"
        st -e $CMD
      '';
    in
    {
      home.packages = [ script ];
      programs.fish.shellInit = ''
        function __rofi-command-hook --on-event fish_postexec --description 'add command to command frecency history'
          if test $status -eq 0
            frecently $HOME/.local/share/rofi/command-frecency bump "$argv"
          end
        end
      '';
    };
in
{
  imports = [
    rofi-directory
    rofi-command
  ];
  home.packages = [
    pkgs.rofi-power-menu
    pkgs.rofi-systemd
    pkgs.rofimoji
    # pkgs.rofi-vpn
    # pkgs.rofi-mpd
    pkgs.rofi-pass
    rofi-web-search
    frecently
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
