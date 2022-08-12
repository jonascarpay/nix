{ pkgs, ... }:
let

  org-dir = "/home/jmc/Org/"; # Mirrored in config.el
  history-root = "/home/jmc/.local/share/frecently";

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

  org-bookmarks = pkgs.writeShellScriptBin "org-bookmarks" ''
    ${pkgs.python3}/bin/python ${python-compile ./bookmarks.py} ${org-dir} ${history-root}/org-bookmarks-history
  '';

  org-open = let history = "${history-root}/org-open"; in
    pkgs.writeShellScriptBin "org-open" ''
      set -e
      frecently view ${history} | while read -r file; do
        if [ ! -e "${org-dir}/$file.org" ]; then
          echo "Removing $file" 
          frecently delete ${history} "$file"
        fi
      done
      if [[ $1 == "-today" ]]; then
        extraflags=(-it "$(date +"%Y-%m-%d")-")
        shift
      fi
      FILE=$(find ${org-dir}/ -type f -name '*.org' | sed -E 's#${org-dir}/(.*)\.org#\1#' | frecently view ${history} -a | dmenu -i -p " " "''${extraflags[@]}")
      frecently bump ${history} "$FILE"
      SUBDIR="${org-dir}/$(echo "$FILE" | sed -E 's#(.*/)*.*#\1#')"
      mkdir -p "$SUBDIR"
      emacs --chdir ${org-dir} "${org-dir}/$FILE.org"
    '';

  org-todo-csv = pkgs.writeScriptBin "org-todo-csv" ''
    #!/usr/bin/env doomscript

    (defcli! list-todos ()
      (setq org-agenda-files '("${org-dir}"))
      (print! (org-batch-agenda-csv "t")))

    (run! "list-todos" ())
  '';

in
{
  home.packages = [
    org-open
    org-todo-csv
    org-bookmarks
  ];
}
