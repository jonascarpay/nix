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
        shift
      fi
      NOTE=$(find ${note-dir}/ -type f -name '*.md' | sed -E 's#${note-dir}/(.*).md#\1#' | frecently view ${history} -a | dmenu -i -p " " "''${extraflags[@]}")
      frecently bump ${history} "$NOTE"
      SUBDIR="${note-dir}/$(echo "$NOTE" | sed -E 's#(.*/)*.*#\1#')"
      mkdir -p "$SUBDIR"
      st $@ -d ${note-dir} -e vim + "$NOTE.md"
    '';

  note-today = pkgs.writeShellScriptBin "note-today" ''
    set -e
    DIR=${note-dir}/Daily
    mkdir -p $DIR
    st -d ${note-dir} -e vim + "$DIR/$(date +"%Y-%m-%d").md"
  '';

  python-compile = src: pkgs.stdenv.mkDerivation {
    # This drops the /nix/store prefix
    name = "compile-${builtins.substring 44 (-1) src}";
    unpackPhase = "true";
    installPhase = ''
      cp ${src} main.py
      ${pkgs.python3}/bin/python -m py_compile main.py
      mv __pycache__/* $out
    '';
  };
  note-bookmarks = pkgs.writeShellScriptBin "note-bookmarks" ''
    ${pkgs.python3}/bin/python ${python-compile ./bookmarks.py} ${note-dir} ${history-root}/note-bookmarks-history
  '';

  note-todos = pkgs.writeShellScriptBin "note-todos" ''
    ${pkgs.python3}/bin/python ${python-compile ./todos.py} ${note-dir} $@
  '';

  todo-increment = pkgs.writeShellScriptBin "todo-increment" ''
    ${pkgs.python3}/bin/python ${python-compile ./todo_increment.py}
  '';

in
{
  nixpkgs.overlays = [ (final: prev: { inherit note-todos; }) ];
  home.packages = [
    note-bookmarks
    note-open
    note-today
    note-todos
    todo-increment
  ];
}
