{ pkgs, ... }:
let
  dm-mono-nerd-font = pkgs.stdenv.mkDerivation rec {
    name = "dm-mono-nerd-font";
    # TODO flakify
    src = pkgs.fetchFromGitHub {
      repo = name;
      owner = "ylieder";
      rev = "474a1f45cac18791dd6fd8a9f81732b0e8779717";
      sha256 = "sha256-CzaQMqJuMU1OmOjjVfAeb1nzhxy7zmQozjGSk510o5Q=";
    };
    installPhase = ''
      mkdir -p $out/share/fonts/truetype
      cp DMMonoNerdFont/* $out/share/fonts/truetype
    '';
  };
in
{
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      corefonts
      google-fonts
      liberation_ttf
      # TODO prune using nerdfonts.override, see https://nixos.wiki/wiki/Fonts
      nerdfonts
      powerline-fonts
      dm-mono-nerd-font
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
