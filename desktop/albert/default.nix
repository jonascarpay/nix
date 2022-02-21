{ pkgs, ... }: {
  # this provides kfmtool, which it complains about needing to open web browsers
  # why is that necessary? no clue
  home.packages = [
    pkgs.libsForQt5.konqueror
  ];
  home.file = {
    albertConf.target = ".config/albert/albert.conf";
    albertConf.source = ./albert.conf;
    engines.target = ".config/albert/org.albert.extension.websearch/engines.json";
    engines.source = ./engines.json;
  };
  xsession.initExtra = ''
    ${pkgs.albert}/bin/albert &
  '';
}
