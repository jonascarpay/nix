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
in
{
  imports = [
    ./dunst.nix
    ./emacs
    # ./neuron.nix
    ./picom.nix
    ./polybar.nix
    ./dmenu
    ./redshift.nix
    ./random-wallpaper.nix
    # ./xmonad
    ./i3
    # ./notes
    widgets
  ];
  home.packages = with pkgs; [
    inputs.st.defaultPackage.${pkgs.system}
    inputs.blender.packages.${pkgs.system}.default
    anki-bin
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

  xsession.enable = true;
}
