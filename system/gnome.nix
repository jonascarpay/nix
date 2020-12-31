{ pkgs, config, ... }:
let
  unstable = import <unstable> {};
  matshell = pkgs.stdenv.mkDerivation {
    pname = "material-shell";
    version = "4";
    src = pkgs.fetchFromGitHub {
      owner = "material-shell";
      repo = "material-shell";
      rev = "688ce24dd5c829d994bf10c8a79a6efd78901a4d";
      sha256 = "19z0l1vhz5b2jk3s33zxdrvzqlwhxyaxwzg250wldlglanz9phb8";
    };
    buildPhase = "true";
    installPhase = ''
      mkdir -p $out/share/gnome-shell/extensions
      cp -r $src $out/share/gnome-shell/extensions/material-shell@papyelgringo
    '';
  };
  ppwm = unstable.gnomeExtensions.paperwm.overrideAttrs (
    old:
      {
        src = pkgs.fetchFromGitHub {
          owner = "paperwm";
          repo = "PaperWM";
          rev = "40a750918845f3c708dd1b51a4";
          sha256 = "01r2ifwrl8w735d0ckzlwhvclax9dxd2ld5y2svv5bp444zbjsag";
        };
      }
  );

in
{
  environment.systemPackages = with pkgs; [
    gnome3.eog
    gnome3.evince
    gnome3.gnome-tweak-tool
    # matshell
    # ppwm
    gnomeExtensions.paperwm
  ];
  services = {
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
    };

    dbus.packages = [ pkgs.gnome3.dconf ];
    udev.packages = [ pkgs.gnome3.gnome-settings-daemon ];
  };
}
