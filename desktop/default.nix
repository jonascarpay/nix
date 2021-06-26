{ pkgs, config, ... }:
{
  imports = [
    ./emacs
    ./picom.nix
    ./anki.nix
    ./polybar.nix
    ./redshift.nix
    ./blender.nix
    ./st
    ./xmonad
    ./albert
    ./neuron.nix
  ];
  home.packages = with pkgs; [
    config.channels.unstable.darktable
    celluloid
    gnome3.nautilus
    okular
    discord
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
    dunst = {
      enable = true;
      settings = {
        colors.foreground = "#d8dee9";
        global = {
          follow = "keyboard";
          geometry = "900-15+45"; # tweaked for bar size 30
          # shrink = true; # i want this but width calculation is broken? spotify notifications get line breaks for some reason
          padding = 8; # vertical
          horizontal_padding = 8;
          text_icon_padding = 16;
          markup = "full";
          font = "SauceCodePro Nerd Font 10";
          format = "<b>%s</b>\\n\\n%b";
          max_icon_size = 160;
          word_wrap = true;
        };
        urgency_low.background = "#232831";
        urgency_normal.background = "#232831";
        urgency_critical.background = "#4c566a";
      };
    };
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
    gtk3.extraCss = ''
      VteTerminal, vte-terminal {
        padding: 9px;
      }
    ''; # Termite padding goes here
  };

}
