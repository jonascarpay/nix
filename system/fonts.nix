{ pkgs, ... }:
let
  dm-mono = pkgs.stdenv.mkDerivation {
    name = "dm-mono";
    src = pkgs.fetchFromGitHub {
      owner = "googlefonts";
      repo = "dm-mono";
      rev = "57fadabfb200a77de2812540026c249dc3013077";
      sha256 = "07y3csk0vy3b3mq33bb73m63p9vyk8dhf6ysqnxabdpvy6d98gjy";
    };
    buildPhase = "true";
    installPhase = ''
      mkdir -p $out/share/doc/dm-mono
      mkdir -p $out/fonts/truetype
      cp AUTHORS.txt CHANGELOG.md $out/share/doc/dm-mono
      cp exports/* $out/fonts/truetype
    '';
  };
in
{
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      corefonts
      dm-mono
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
