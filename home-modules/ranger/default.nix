{ pkgs, ... }:

{

  home.packages = [ pkgs.ranger ];
  home.file.rangerConf = {
    target = ".config/ranger/rc.conf";
    source = ./rc.conf;
  };

}
