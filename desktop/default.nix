{ pkgs, ... }:
{
  imports = [
    ./blender.nix
    ./emacs
    ./picom.nix
    ./polybar.nix
    ./wal.nix
    ./redshift.nix
    ./st
    ./xmonad
    ./albert
  ];
  home.packages = with pkgs; [
    anki
    celluloid
    gnome3.nautilus
    okular
    pavucontrol
    signal-desktop
    skype
    slack
    spotify
    steam
    sxiv
    transmission-gtk
    xclip # Doesn't work?
  ];

  home.sessionVariables.TERMCMD = "termite";

  programs.termite.enable = true;
  programs.firefox.enable = true;
  home.keyboard.options = [ "ctrl:nocaps" ];
  qt.enable = true;
  fonts.fontconfig.enable = true;
  services = {
    flameshot.enable = true;
    dropbox.enable = true;
    unclutter.enable = true;
    caffeine.enable = true;
    dunst.enable = true;
  };

  services.syncthing.enable = true;
  services.syncthing.tray = true;
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
    gtk3.extraCss = ''
      VteTerminal, vte-terminal {
        padding: 9px;
      }
    ''; # Termite padding goes here
  };

}
