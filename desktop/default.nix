{ pkgs, config, ... }:
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
  ];
  home.packages = with pkgs; [
    celluloid
    pkgs.unstable.darktable
    discord
    gnome3.nautilus
    okular
    pavucontrol
    signal-desktop
    skype
    slack
    spotify
    steam
    sxiv
    tdesktop
    transmission-gtk
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

  programs.git.ignores = [
    ".stversions"
    "*.sync-confict-*"
  ];

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
    # gtk3.extraCss = ''
    #   VteTerminal, vte-terminal {
    #     padding: 9px;
    #   }
    # ''; # Termite padding goes here
  };

}
