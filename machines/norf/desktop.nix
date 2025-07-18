{ pkgs, unstable, ... }:
let
  polybar = {
    imports = [ ../../desktop/polybar.nix ];
    services.polybar.settings = {
      "bar/mybar" = {
        "inherit" = "bar/common bar/hidpi";
        modules-right = "notifications mochi onigiri wired vpn fs memory cpu-temp gpu-temp cpu pulseaudio date-nl date";
      };
      "module/wireless".interface = "wlp9s0";
      "module/wired".interface = "eno1";
      "module/cpu-temp".hwmon-path = "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon2/temp1_input";
      "module/gpu-temp".hwmon-path = "/sys/devices/pci0000:00/0000:00:08.1/0000:0d:00.0/hwmon/hwmon5/temp1_input";
    };
  };

  myretroarch = pkgs.retroarch.override { cores = [ pkgs.libretro.mupen64plus ]; };

  myanki = pkgs.writeShellScriptBin "anki" ''
    export PATH="${pkgs.lib.makeBinPath [ pkgs.mpv ]}:$PATH"
    export ANKI_WEBSCALE=2
    ${pkgs.anki-bin}/bin/anki $@
  '';

  myocr = pkgs.writeShellScriptBin "ocr-capture" ''
    set -euo pipefail
    TMP=$(mktemp -d)
    trap 'rm -rf $TMP' EXIT
    ${pkgs.xfce.xfce4-screenshooter}/bin/xfce4-screenshooter --region --save $TMP/cap.png
    ${pkgs.imagemagick}/bin/convert $TMP/cap.png -resize 300% $TMP/cap_upscale.png
    ${pkgs.tesseract}/bin/tesseract $TMP/cap_upscale.png stdout -l eng+jpn | sed -z 's/[[:space:]]*$//g' | xclip -selection clipboard
  '';

  pass = {
    imports = [ ../../home/pass.nix ];
    programs.password-store.settings.PASSWORD_STORE_DIR = "/home/jmc/Passwords";
    home.packages = [ pkgs.qtpass ];
  };
in

{
  imports = [
    ../../nixos/home-manager-xsession.nix
    ../../nixos/syncthing-desktop.nix
    ../../nixos/fonts.nix
    ../../desktop/jp.nix
  ];
  home-manager.users.jmc.imports = [
    ../../desktop
    polybar
    pass
  ];
  home-manager.users.jmc.home.packages = [
    myretroarch
    myanki
    myocr
    pkgs.qbittorrent
    pkgs.slack
    unstable.signal-desktop
    pkgs.tdesktop
    pkgs.ungoogled-chromium
    pkgs.google-chrome
    pkgs.blender
    pkgs.darktable
    pkgs.steam
    pkgs.spotify
    pkgs.discord
  ];
}
