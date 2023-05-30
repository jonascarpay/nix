{ pkgs, inputs, config, ... }:
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

  services.redshift = {
    enable = true;
    tray = true;
    latitude = "35.6762";
    longitude = "139.6503";
  };

  xsession.enable = true;
}
