{ pkgs, unstable, config, ... }:
let
  syncthing = {
    services.syncthing = {
      enable = true;
      tray.enable = true;
      tray.command = "syncthingtray --wait";
    };
    programs.git.ignores = [
      ".stversions"
      "*.sync-confict-*"
      ".stignore"
      ".stfolder"
    ];
  };
in
{
  imports = [
    ./albert
    ./anki.nix
    ./blender.nix
    ./dunst.nix
    ./emacs
    ./neuron.nix
    ./picom.nix
    ./polybar.nix
    ./redshift.nix
    ./st
    ./xmonad
    syncthing
  ];
  home.packages = with pkgs; [
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
