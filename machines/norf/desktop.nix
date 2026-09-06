{ pkgs, unstable, ... }:
let
  obsidian = { pkgs, config, ... }: {
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

  pass = {
    imports = [ ../../home/pass.nix ];
    programs.password-store.settings.PASSWORD_STORE_DIR = "/home/jmc/Passwords";
    home.packages = [ pkgs.qtpass ];
  };


  wallpaper = {
    imports = [ ../../desktop/random-wallpaper.nix ];
    services.randomWallpaper = {
      enable = true;
      wallpaperPath = "/home/jmc/Wallpapers/papes";
    };
  };
in
{
  imports = [
    ../../nixos/fonts.nix
    ../../desktop/jp.nix # TODO probably inline
    ../../desktop
  ];

  programs.firefox.enable = true;

  home-manager.users.jmc = { config, ... }: {

    imports = [
      wallpaper
      obsidian
      pass
      ../../desktop/qalculate.nix
    ];

    home.packages = [
      unstable.spotify
      pkgs.telegram-desktop
      pkgs.kdePackages.okular
      unstable.signal-desktop
      unstable.blender
      pkgs.teams-for-linux
    ];

    programs.neovide.settings.font.size = 14.0;
    programs.alacritty.settings.font.size = 14;
    programs.niri.settings = {
      window-rules = [
        {
          matches = [{ app-id = "Alacritty"; }];
          default-column-width.proportion = 0.333;
        }
        {
          matches = [{ app-id = "neovide"; }];
          default-column-width.proportion = 0.667;
        }
      ];
      layout = {
        gaps = 32;
        struts.left = 32;
        struts.right = 32;
      };
    };

    services.swayidle = {
      enable = true;
      systemdTargets = [ "niri.service" ];
      timeouts = [{
        timeout = 600;
        command = "${config.programs.niri.package}/bin/niri msg action power-off-monitors";
        resumeCommand = "${config.programs.niri.package}/bin/niri msg action power-on-monitors";
      }];
    };

    # Spotify (and other media players) don't request idle inhibition
    # themselves, so watch pipewire for playback instead. niri only honours
    # wayland idle inhibitors attached to a *visible* surface, hence the d-bus
    # (org.freedesktop.ScreenSaver) backend.
    systemd.user.services.pipewire-idle-inhibit = {
      Unit = {
        Description = "Inhibit idle while media is playing through pipewire";
        PartOf = [ "niri.service" ];
        After = [ "niri.service" "pipewire.service" ];
        Wants = [ "pipewire.service" ];
      };
      Service = {
        ExecStart = "${pkgs.wayland-pipewire-idle-inhibit}/bin/wayland-pipewire-idle-inhibit --idle-inhibitor d-bus --media-minimum-duration 30";
        Restart = "always";
        RestartSec = 10;
      };
      Install.WantedBy = [ "niri.service" ];
    };

    services.wlsunset = {
      enable = true;
      latitude = "35.6762";
      longitude = "139.6503";
      systemdTarget = "niri.service";
      temperature.night = 4500;
    };

  };
}
