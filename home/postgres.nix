{ pkgs, ... }: {
  home.sessionVariables = {
    PSQL_PAGER = "${pkgs.pspg}/bin/pspg --reprint-on-exit";
  };
  home.packages = [ pkgs.pgcli pkgs.pspg ];
  xdg.configFile."pgcli/config".text = ''
    [main]

    # https://github.com/okbob/pspg?tab=readme-ov-file#pgcli
    pager = ${pkgs.pspg}/bin/pspg --rr=2 --quit-if-one-screen --ignore-case --reprint-on-exit

    # Set threshold for row limit. Use 0 to disable limiting.
    row_limit = 10000

    less_chatty = True

    prompt = '\H:\d> '

    keyring = False
  '';
  home.file.".psqlrc".text = ''
    \set QUIET 1
    \pset linestyle unicode
    \pset border 2
    \unset QUIET
  '';
}
