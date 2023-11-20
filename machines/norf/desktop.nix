{ pkgs, ... }:
let
  polybar = {
    imports = [ ../../desktop/polybar.nix ];
    services.polybar.settings = {
      "bar/mybar" = {
        "inherit" = "bar/common bar/hidpi";
        modules-right = "notifications onigiri wired vpn fs memory cpu-temp gpu-temp cpu pulseaudio date-nl date";
      };
      "module/wireless".interface = "wlp9s0";
      "module/wired".interface = "eno1";
      "module/cpu-temp".hwmon-path = "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon2/temp1_input";
      "module/gpu-temp".hwmon-path = "/sys/devices/pci0000:00/0000:00:08.1/0000:0d:00.0/hwmon/hwmon5/temp1_input";
    };
  };

  redshift = {
    services.redshift = {
      enable = true;
      tray = true;
      latitude = "35.6762";
      longitude = "139.6503";
    };
  };

  myretroarch = pkgs.retroarch.override { cores = [ pkgs.libretro.mupen64plus ]; };

  myanki = pkgs.writeShellScriptBin "anki" ''
    export PATH="${pkgs.lib.makeBinPath [ pkgs.mpv ]}:$PATH"
    export ANKI_WEBSCALE=2
    ${pkgs.anki}/bin/anki
  '';

  myocr = pkgs.writeShellScriptBin "ocr-capture" ''
    set -euo pipefail
    TMP=$(mktemp -d)
    trap 'rm -rf $TMP' EXIT
    ${pkgs.xfce.xfce4-screenshooter}/bin/xfce4-screenshooter --region --save $TMP/cap.png
    ${pkgs.imagemagick}/bin/convert $TMP/cap.png -resize 300% $TMP/cap_upscale.png
    ${pkgs.tesseract}/bin/tesseract $TMP/cap_upscale.png stdout -l eng+jpn | sed -z 's/[[:space:]]*$//g' | xclip -selection clipboard
  '';

  myrehex = (pkgs.writeShellScriptBin "rehex" ''
    export GIO_EXTRA_MODULES='${pkgs.dconf}/lib/gio/modules'
    export GDK_PIXBUF_MODULE_FILE='${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache'
    export XDG_DATA_DIRS='${pkgs.gtk3}/share/gsettings-schemas/gtk+3-${pkgs.gtk3.version}:${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/gsettings-desktop-schemas-${pkgs.gsettings-desktop-schemas.version}'
    ${pkgs.rehex}/bin/rehex "$@"
  '');

  pass = {
    imports = [ ../../home/pass.nix ];
    programs.password-store.settings.PASSWORD_STORE_DIR = "/home/jmc/Passwords";
    home.packages = [ pkgs.qtpass ];
  };

  thunar = {
    services.tumbler.enable = true;
    programs.thunar.enable = true;
    programs.thunar.plugins = [
      pkgs.xfce.thunar-archive-plugin
      pkgs.xfce.thunar-volman
    ];
  };

in

{
  imports = [
    ../../nixos/home-manager-xsession.nix
    ../../nixos/syncthing-desktop.nix
    ../../nixos/fonts.nix
    ../../desktop/jp.nix
    thunar
  ];
  home-manager.users.jmc.imports = [
    ../../desktop
    ../../desktop/emacs
    polybar
    redshift
    pass
  ];
  home-manager.users.jmc.home.packages = [
    myretroarch
    myanki
    myocr
    myrehex
    pkgs.slack
    pkgs.signal-desktop
    pkgs.tdesktop
    pkgs.ungoogled-chromium
    pkgs.blender
    pkgs.darktable
    pkgs.steam
    pkgs.spotify
    pkgs.discord
  ];
}
