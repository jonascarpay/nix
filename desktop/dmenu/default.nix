{ pkgs, inputs, ... }:
let
  history-root = "~/.local/share/frecently";

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
          DIR=$(find $HOME -mindepth 1 -maxdepth 2 -type d -not -path '*/.*' | frecently view ${history} -a | sed "s#$HOME#~#" | dmenu -i -p " ")
          DIR_REAL=$(realpath "''${DIR/#\~/$HOME}")
          if [ -d $DIR_REAL ]; then
            frecently bump ${history} "$DIR_REAL"
            st -d "$DIR_REAL" fish
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

  note-dir = "/home/jmc/Documents/Notes";
  dmenu-notes =
    let
      history = "${history-root}/notes-history";
    in
    pkgs.writeShellScriptBin "dmenu-notes" ''
      set -e
      for note in $(frecently view ${history}); do
        if [ ! -e "${note-dir}/$note.md" ]; then
          echo "Removing $note" 
          frecently delete ${history} "$note"
        fi
      done
      NOTE=$(find ${note-dir}/ -type f -name '*.md' | sed -E 's#${note-dir}/(.*).md#\1#' | frecently view ${history} -a | dmenu -i -p " ")
      frecently bump ${history} "$NOTE"
      SUBDIR="${note-dir}/$(echo "$NOTE" | sed -E 's#(.*/)*.*#\1#')"
      mkdir -p "$SUBDIR"
      st -d ${note-dir} -e vim "$NOTE.md"
    '';

  dmenu-note-bookmarks =
    let
      history = "${history-root}/note-bookmark-history";
    in
    pkgs.writeShellScriptBin "dmenu-note-bookmarks" ''
      set -e
      declare -A links

      while read -r link; do
        title=$(echo $link | sed -E 's#${note-dir}/(.*)\.md:\[(.*)\].*#\1 - \2#')
        url=$(echo $link | sed -E 's#${note-dir}/.*\.md:\[.*\]\((.*)\)#\1#')
        links["$title"]="$url"
      done < <(grep --recursive --only-matching '\[.*\]\(.*\)' ${note-dir}/)

      gen_list() {
        for i in "''${!links[@]}"; do
          echo $i
        done
      }

      PICK=$(gen_list | frecently view ${history} -ar | dmenu -i -sr -p " ")
      frecently bump ${history} "$PICK"
      xdg-open "''${links["$PICK"]}"
    ''
  ;

  dmenu-command =
    let
      history = "${history-root}/command-history";
      script = pkgs.writeShellScriptBin "dmenu-command" ''
        set -e
        CMD=$(frecently view ${history} | dmenu -p " ")
        frecently bump "${history}" "$CMD"
        st -e $CMD
      '';
    in
    {
      home.packages = [ script ];
      programs.fish.shellInit = ''
        function __rofi-command-hook --on-event fish_postexec --description 'add command to command frecency history'
          # Only accept if the status was success, and it was a one-liner
          if test $status -eq 0 -a (echo "$argv" | wc -l) -eq 1
            frecently bump ${history} "$argv"
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
    frecently view "$@" | dmenu -mr -p " " | xargs frecently delete "$@"
  '';

in
{
  imports = [
    dmenu-directory
    dmenu-command
  ];
  home.packages = [
    inputs.dmenu.defaultPackage.${pkgs.system}
    inputs.frecently.defaultPackage.${pkgs.system}
    dmenu-run
    dmenu-pass
    dmenu-web-search
    dmenu-delete
    dmenu-notes
    dmenu-note-bookmarks
  ];
}
