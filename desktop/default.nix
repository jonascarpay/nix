{ pkgs, inputs, ... }:
let
  widgets = {
    qt.enable = true;
    gtk = {
      enable = true;
    };
    # TODO what's this for?
    xdg.systemDirs.data = [
      "${pkgs.gtk3}/share/gsettings-schemas/gtk+3-${pkgs.gtk3.version}"
      "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/gsettings-desktop-schemas-${pkgs.gsettings-desktop-schemas.version}"
    ];
  };

  qalculate = {
    home.packages = [ pkgs.qalculate-gtk ];
    xsession.windowManager.i3.config.floating.criteria = [{ class = "Qalculate-gtk"; }];
  };

  zap = pkgs.writeShellScriptBin "zap" ''
    xprop | grep -oP "PID\(CARDINAL\) = \K\d+" | xargs kill -9
  '';

  cursor = {
    home.pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      gtk.enable = true;
      x11.enable = true;
    };
  };

  # TODO why doesn't firefox pick up light in the same way
  # TODO set up darkman
  dark-mode = {
    dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
    gtk.iconTheme = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
    };
    gtk.theme = {
      package = pkgs.gnome.gnome-themes-extra;
      name = "Adwaita-dark";
    };
  };

in
{
  imports = [
    ./dunst.nix
    ./picom.nix
    ./dmenu
    ./random-wallpaper.nix
    ./i3
    widgets
    qalculate
    dark-mode
    cursor
  ];
  home.packages = [
    inputs.st.defaultPackage.${pkgs.system}
    pkgs.vlc
    pkgs.gnome3.nautilus
    pkgs.okular
    pkgs.sxiv
    pkgs.xclip # Doesn't work?
    zap
  ];

  programs.firefox.enable = true;
  home.keyboard.options = [ "ctrl:nocaps" ];
  fonts.fontconfig.enable = true;
  services = {
    flameshot.enable = true;
    unclutter.enable = true;
    caffeine.enable = true;
  };

}
