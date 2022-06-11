{ pkgs, ... }:
let
  note-dir = "/home/jmc/Documents/Notes";
  history-root = "/home/jmc/.local/share/frecently";

  note-open =
    let
      history = "${history-root}/notes-history";
    in
    pkgs.writeShellScriptBin "note-open" ''
      set -e
      frecently view ${history} | while read -r note; do
        if [ ! -e "${note-dir}/$note.md" ]; then
          echo "Removing $note" 
          frecently delete ${history} "$note"
        fi
      done
      if [[ $1 == "-today" ]]; then
        extraflags=(-it "$(date +"%Y-%m-%d")-")
      fi
      NOTE=$(find ${note-dir}/ -type f -name '*.md' | sed -E 's#${note-dir}/(.*).md#\1#' | frecently view ${history} -a | dmenu -i -p " " "''${extraflags[@]}")
      frecently bump ${history} "$NOTE"
      SUBDIR="${note-dir}/$(echo "$NOTE" | sed -E 's#(.*/)*.*#\1#')"
      mkdir -p "$SUBDIR"
      st -d ${note-dir} -e vim + "$NOTE.md"
    '';

  note-today = pkgs.writeShellScriptBin "note-today" ''
    set -e
    DIR=${note-dir}/Daily
    mkdir -p $DIR
    st -d ${note-dir} -e vim + "$DIR/$(date +"%Y-%m-%d").md"
  '';

  note-todos = pkgs.writeShellScriptBin "note-todos" ''
    set -e
    declare -A files
    declare -A lines

    while read -r line; do
      file=$(echo $line | sed -E 's#${note-dir}/(.*):[0-9]+:.*#\1#')
      linenr=$(echo $line | sed -E 's#${note-dir}/.*:([0-9]+):.*#\1#')
      title=$(echo $line | sed -E 's#${note-dir}/(.*)\.md:[0-9]+:-\s*\[\s+\]\s*(.*)#\1 - \2#')
      files["$title"]="$file"
      lines["$title"]="$linenr"
    done < <(grep --line-number --recursive "^-\s*\[\s\+\]" ${note-dir})

    gen_list() {
      for i in "''${!files[@]}"; do
        echo $i
      done
    }

    PICK=$(gen_list | dmenu -i -sr -p " ")
    st -d ${note-dir} -e vim "+''${lines["$PICK"]}" "''${files["$PICK"]}"
  '';


  note-bookmarks = pkgs.writeShellScriptBin "note-bookmarks" ''
    ${pkgs.python3}/bin/python ${./bookmarks.py} ${note-dir} ${history-root}/note-bookmarks-history
  '';
in
{
  home.packages = [
    note-bookmarks
    note-open
    note-today
    note-todos
  ];
}
