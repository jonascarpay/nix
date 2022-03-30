{ pkgs, unstable, config, ... }:
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
in
{
  imports = [
    ./blender.nix
    ./dunst.nix
    ./rofi.nix
    ./emacs
    ./neuron.nix
    ./picom.nix
    ./polybar.nix
    ./dmenu
    ./redshift.nix
    ./st
    ./xmonad
    widgets
  ];
  home.packages = with pkgs; [
    unstable.anki-bin
    celluloid
    discord
    gnome3.nautilus
    google-chrome
    gparted
    okular
    pavucontrol
    signal-desktop
    slack
    spotify
    steam
    sxiv
    unstable.tdesktop
    transmission-gtk
    ungoogled-chromium
    unstable.darktable
    xclip # Doesn't work?
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
  services.random-background = {
    enable = true;
    imageDirectory = "%h/Wallpapers/papes";
    interval = "1h";
  };
  services.syncthing = {
    enable = true;
    tray.enable = true;
    tray.command = "syncthingtray --wait";
  };

  xsession.enable = true;
}
