{ pkgs, inputs, unstable, config, ... }:
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
    ./emacs
    ./neuron.nix
    ./picom.nix
    ./polybar.nix
    ./dmenu
    ./redshift.nix
    ./random-wallpaper.nix
    ./xmonad
    widgets
  ];
  home.packages = with pkgs; [
    inputs.st.defaultPackage.${pkgs.system}
    unstable.anki-bin
    celluloid
    discord
    gnome3.nautilus
    gnome3.gnome-calculator
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
  services.syncthing = {
    enable = true;
    tray.enable = true;
    tray.command = "syncthingtray --wait";
  };

  xsession.enable = true;
}
