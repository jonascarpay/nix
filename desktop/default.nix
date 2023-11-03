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
  ];
  home.packages = [
    inputs.st.defaultPackage.${pkgs.system}
    pkgs.vlc
    pkgs.gnome3.nautilus
    pkgs.okular
    pkgs.sxiv
    pkgs.xclip # Doesn't work?
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
