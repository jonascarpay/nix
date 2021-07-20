{ pkgs, ... }:
{
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      corefonts
      google-fonts
      liberation_ttf
      nerdfonts
      powerline-fonts
      tewi-font
    ];
    fontconfig = {
      enable = true;
      allowBitmaps = false;
      defaultFonts = {
        monospace = [
          # "SauceCodePro Nerd Font Complete"
          "SauceCodePro Nerd Font"
          # TODO move to jp.nix?
          "IPAGothic"
        ];
        sansSerif = [ "DejaVu Sans" "IPAPGothic" ];
        serif = [ "DejaVu Serif" "IPAPMincho" ];
      };
    };
  };

}
