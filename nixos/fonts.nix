{ pkgs, inputs, ... }:
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
  # dm-mono-nerd-font = pkgs.stdenv.mkDerivation rec {
  #   name = "dm-mono-nerd-font";
  #   # TODO flakify
  #   src = pkgs.fetchFromGitHub {
  #     repo = name;
  #     owner = "minhuw";
  #     rev = "e11d1c3152df52045aaf9fa06da7a03958c4139a";
  #     sha256 = "sha256-NgxDw7IwOOkHG545S+0biX2cBkP0EPdwLHzP0QYSrTk=";
  #   };
  #   installPhase = ''
  #     mkdir -p $out/share/fonts/truetype
  #     cp dm-mono-nerd-font/* $out/share/fonts/truetype
  #   '';
  # };
in
{
  fonts = {
    # fontDir.enable = true; # creates /run/current-system/sw/share/X11/fonts. Not sure why needed
    enableDefaultPackages = true;
    packages = [
      pkgs.noto-fonts
      pkgs.noto-fonts-cjk-sans
      pkgs.noto-fonts-cjk-serif
      pkgs.noto-fonts-emoji
      pkgs.liberation_ttf
      # See https://github.com/NixOS/nixpkgs/blob/6998cf86e9a6ef83b32956337f65aba8656671fe/pkgs/data/fonts/nerdfonts/shas.nix
      (pkgs.nerdfonts.override { fonts = [ "SourceCodePro" ]; })
      # powerline-fonts
      (inputs.dmmono.packages.${pkgs.system}.dm-mono-patched)
      # dm-mono-nerd-font
      # tewi-font
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = [ "Noto Color Emoji" ];
        monospace = [
          "SauceCodePro Nerd Font Mono"
          "Noto Sans Mono"
          # TODO move to jp.nix?
          "Noto Sans Mono CJK JP"
        ];
        sansSerif = [
          "Noto Sans"
          "Noto Sans CJK JP"
        ];
        serif = [
          "Noto Serif"
          "Noto Serif CJK JP"
        ];
      };
    };
  };

}
