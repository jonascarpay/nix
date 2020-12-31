{ pkgs, config, lib, ... }: {
  xsession.windowManager.xmonad = lib.mkIf (config.xsession.enable) {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad.hs;
    extraPackages = hpkgs: [ pkgs.wmctrl hpkgs.dbus ];
  };
}
