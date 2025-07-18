{ pkgs, config, ... }:
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
    home = {
      # https://github.com/nix-community/home-manager/issues/3113
      packages = [ pkgs.dconf ]; # TODO should be enabled through nixos with programs.dconf.enable
      pointerCursor = {
        package = pkgs.vanilla-dmz;
        name = "Vanilla-DMZ";
        gtk.enable = true;
        x11.enable = true;
      };
    };
  };

  obsidian = {
    home.packages = [
      pkgs.obsidian
    ];
    services.git-sync = {
      enable = true;
      repositories.obsidian = {
        path = "${config.home.homeDirectory}/Obsidian";
        uri = "git+ssh://git@github.com:jonascarpay/Obsidian.git";
      };
    };
  };

  # TODO why doesn't firefox pick up light in the same way
  # TODO set up darkman
  dark-mode = {
    dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
    gtk.iconTheme = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };
    gtk.theme = {
      package = pkgs.gnome-themes-extra;
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
    obsidian
  ];
  home.packages = [
    (pkgs.callPackage ./st { })
    pkgs.vlc
    pkgs.nautilus
    # pkgs.okular
    pkgs.sxiv
    pkgs.xclip # Doesn't work?
    pkgs.teams-for-linux
    zap
  ];

  home.sessionVariables.ST_FONT = "DM Mono Nerd Font:pixelsize=24:antialias=true:autohint=true";

  programs.firefox.enable = true;
  home.keyboard.options = [ "ctrl:nocaps" ];
  fonts.fontconfig.enable = true;
  services = {
    flameshot.enable = true;
    unclutter.enable = true;
    caffeine.enable = true;
  };

}
