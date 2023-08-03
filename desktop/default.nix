{ pkgs, inputs, ... }:
let
  widgets = {
    home.packages = [ pkgs.breeze-icons ];
    qt.enable = true;
    gtk = {
      enable = true;
      theme = {
        package = pkgs.numix-gtk-theme;
        name = "Numix";
      };
      iconTheme = {
        package = pkgs.numix-icon-theme;
        name = "Numix";
      };
    };
  };

  anki-wrapped = pkgs.writeShellScriptBin "anki" ''
    export PATH="${pkgs.lib.makeBinPath [ pkgs.mpv ]}:$PATH"
    export ANKI_WEBSCALE=2
    ${pkgs.anki-bin}/bin/anki
  '';

  retroarch = pkgs.retroarch.override {
    cores = [
      pkgs.libretro.mupen64plus
    ];
  };

  ocr = pkgs.writeShellScriptBin "ocr-capture" ''
    set -euo pipefail
    TMP=$(mktemp -d)
    trap 'rm -rf $TMP' EXIT
    ${pkgs.xfce.xfce4-screenshooter}/bin/xfce4-screenshooter --region --save $TMP/cap.png
    ${pkgs.imagemagick}/bin/convert $TMP/cap.png -resize 300% $TMP/cap_upscale.png
    ${pkgs.tesseract}/bin/tesseract $TMP/cap_upscale.png stdout -l eng+jpn | sed -z 's/[[:space:]]*$//g' | xclip -selection clipboard
  '';
in
{
  imports = [
    ./dunst.nix
    ./emacs
    ./picom.nix
    ./polybar.nix
    ./dmenu
    ./random-wallpaper.nix
    # ./xmonad
    ./i3
    # ./notes
    widgets
  ];
  home.packages = with pkgs; [
    inputs.st.defaultPackage.${pkgs.system}
    blender
    anki-wrapped
    celluloid
    discord
    gnome3.nautilus
    qalculate-gtk
    google-chrome
    gparted
    ocr
    okular
    dolphin
    pavucontrol
    signal-desktop
    slack
    spotify
    steam
    sxiv
    tdesktop
    ungoogled-chromium
    darktable
    xclip # Doesn't work?
    retroarch
  ];

  programs.firefox.enable = true;
  home.keyboard.options = [ "ctrl:nocaps" ];
  fonts.fontconfig.enable = true;
  services = {
    flameshot.enable = true;
    # dropbox.enable = true;
    unclutter.enable = true;
    caffeine.enable = true;
  };
  services.syncthing = {
    enable = true;
    tray.enable = true;
    tray.command = "syncthingtray --wait";
  };

  services.redshift =
    let
      tokyo = { latitude = "35.6762"; longitude = "139.6503"; };
      amsterdam = { latitude = "52.372778"; longitude = "4.893611"; };
    in
    {
      inherit (tokyo) latitude longitude;
      enable = true;
      tray = true;
    };

  xsession.enable = true;
}
