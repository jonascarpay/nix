{ pkgs, ... }:
{
  i18n = {
    # defaultLocale = "ja_JP.UTF-8";
    inputMethod = {
      enabled = "fcitx";
      fcitx.engines = with pkgs.fcitx-engines; [ mozc table-other ];
    };
  };

  fonts = {
    fonts = with pkgs; [
      kanji-stroke-order-font
      ipafont
      kochi-substitute
    ];
  };
  environment = {
    systemPackages = [ pkgs.libsForQt5.fcitx-qt5 ];
    variables.XMODIFIER = "@im=fcitx"; # Probably don't need this
  };
}
