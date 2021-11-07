{ pkgs, unstable, config, ... }:
{
  imports = [
    ./albert
    ./blender.nix
    ./dunst.nix
    ./emacs
    ./neuron.nix
    ./picom.nix
    ./polybar.nix
    ./redshift.nix
    ./st
    ./xmonad
  ];
  home.packages = with pkgs; [
    anki-bin
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
    tdesktop
    transmission-gtk
    ungoogled-chromium
    unstable.darktable
    xclip # Doesn't work?
  ];

  home.sessionVariables.TERMCMD = "st";

  programs.firefox.enable = true;
  home.keyboard.options = [ "ctrl:nocaps" ];
  qt.enable = true;
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
}
