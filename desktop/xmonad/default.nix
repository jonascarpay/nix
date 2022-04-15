{ pkgs, config, lib, ... }: {
  home.packages = [
    (pkgs.writeShellScriptBin "clipboard-firefox" ''
      xclip -o | xargs firefox
    '')
  ];
  xsession.windowManager.xmonad = lib.mkIf (config.xsession.enable) {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad.hs;
    extraPackages = hpkgs: [
      pkgs.wmctrl
      hpkgs.dbus
    ];
  };
}
