{ pkgs, ... }:
{
  home-manager.users.jmc.i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = [
      pkgs.fcitx5-mozc
      pkgs.fcitx5-table-other
    ];
  };
  fonts = {
    fonts = with pkgs; [
      kanji-stroke-order-font
      ipafont
      kochi-substitute
    ];
  };
}
