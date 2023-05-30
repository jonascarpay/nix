{ pkgs, config, ... }:
{
  i18n = {
    inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = [
        pkgs.fcitx5-mozc
        pkgs.fcitx5-table-other
      ];
    };
  };

  fonts = {
    fonts = with pkgs; [
      kanji-stroke-order-font
      ipafont
      kochi-substitute
    ];
  };
  # TODO
  # According to nixpkgs@3855e83c49fa2be185c88e56d6c56b40c165b103, there is an XDG autostart file for fcitx, but I don't see it.
  # services.xserver.desktopManager.runXdgAutostartIfNone = true;
  # This just copies back the old launcher.
  systemd.user.services.fcitx5-daemon = {
    enable = true;
    script =
      let
        pkg = config.i18n.inputMethod.package;
      in
      "${pkg}/bin/fcitx5";
    wantedBy = [ "graphical-session.target" ];
  };
}
