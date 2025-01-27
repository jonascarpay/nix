{ pkgs, inputs, ... }:
let
  history-root = "/home/jmc/.local/share/frecently";

  # TODO this does not properly handle directories with spaces in their names
  dmenu-directory =
    let
      history = "${history-root}/directory-history";
      script =
        pkgs.writeShellScriptBin "dmenu-directory" ''
          set -e
          for dir in $(frecently view ${history}); do
            if [ ! -d "$dir" ]; then
              echo "Removing $dir"
              frecently delete ${history} "$dir"
            fi
          done
          DIR=$(find $HOME/Dev $HOME/Documents -maxdepth 1 -type d | frecently view ${history} -a | sed "s#$HOME#~#" | dmenu -w -sep '/' -i -p " ")
          DIR_REAL=$(realpath "''${DIR/#\~/$HOME}")
          if [ -d $DIR_REAL ]; then
            frecently bump ${history} "$DIR_REAL"
            st $@ -d "$DIR_REAL" fish
          fi
        '';
    in
    {
      home.packages = [ script ];
      programs.fish.shellInit = ''
        function __frecently-directory-hook --on-variable PWD --description 'add current directory to directory history'
          frecently bump ${history} "$PWD"
        end
      '';
    };

  dmenu-command =
    let
      fish-history = "${history-root}/fish-command-history";
      dmenu-history = "${history-root}/dmenu-command-history";
      script = pkgs.writeShellScriptBin "dmenu-command" ''
        set -e
        CMD=$(frecently view ${fish-history} | frecently view ${dmenu-history} -a | dmenu -p " ")
        frecently bump "${dmenu-history}" "$CMD"
        st $@ -e fish -c "$CMD; read -n 1 -p 'echo Press any key to continue...' key"
      '';
    in
    {
      home.packages = [ script ];
      programs.fish.shellInit = ''
        function __rofi-command-hook --on-event fish_postexec --description 'add command to command frecency history'
          # Only accept if the status was success, and it was a one-liner
          if test $status -eq 0 -a (echo "$argv" | wc -l) -eq 1
            frecently bump ${fish-history} "$argv"
          end
        end
      '';
    };

  dmenu-run = let history = "${history-root}/run-history"; in
    pkgs.writeShellScriptBin "dmenu-run" ''
      set -e
      CMD=$(dmenu_path | frecently view ${history} -ar | dmenu -sr -p " ")
      frecently bump ${history} "$CMD"
      echo $CMD | ''${SHELL:-"/bin/sh"} &
    '';

  # adapted from https://git.zx2c4.com/password-store/tree/contrib/dmenu/passmenu
  dmenu-pass = let history = "${history-root}/pass-history"; in
    pkgs.writeShellScriptBin "dmenu-pass" ''
      shopt -s nullglob globstar
      set -e

      prefix=$PASSWORD_STORE_DIR
      password_files=( "$prefix"/**/*.gpg )
      password_files=( "''${password_files[@]#"$prefix"/}" )
      password_files=( "''${password_files[@]%.gpg}" )
      password=$(printf '%s\n' "''${password_files[@]}" | frecently view ${history} -ar | dmenu -sr -p "ﳳ " -i)

      [[ -n $password ]] || exit

      frecently bump ${history} "$password"

      pass show "$password" | { IFS= read -r pass; printf %s "$pass"; } | ${pkgs.xdotool}/bin/xdotool type --clearmodifiers --file -
    '';

  dmenu-web-search = pkgs.writeShellScriptBin "dmenu-web-search" (builtins.readFile ./web-search.sh);

  dmenu-delete = pkgs.writeShellScriptBin "dmenu-delete" ''
    set -eo pipefail
    for entry in $(frecently view "$@" | dmenu -mr -p " "); do
      frecently delete "$@" "$entry"
    done
  '';

  dmenu-ssh = pkgs.writeShellScriptBin "dmenu-ssh" ''
    set -eo pipefail
    HISTORY=/home/jmc/.local/share/frecently/ssh-history
    HOST=$(cut -d' ' -f1 ~/.ssh/known_hosts | sed 's/,.*//' | sort -u | frecently view -ar $HISTORY | dmenu -sr -i -p " " -i)
    frecently bump $HISTORY $HOST
    st $@ ssh $HOST
  '';

in
{
  imports = [
    dmenu-directory
    dmenu-command
  ];
  home.packages = [
    (pkgs.callPackage ./dmenu { })
    inputs.frecently.defaultPackage.${pkgs.system}
    dmenu-run
    dmenu-pass
    dmenu-web-search
    dmenu-delete
    dmenu-ssh
  ];
}
