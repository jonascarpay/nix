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
    imports = [ ../../../home/pass.nix ];
    programs.password-store.settings.PASSWORD_STORE_DIR = "/home/jmc/Passwords";
    home.packages = [ pkgs.qtpass ];
  };


  wallpaper = {
    imports = [ ../../../desktop/niri/random-wallpaper.nix ];
    services.randomWallpaper = {
      enable = true;
      wallpaperPath = "/home/jmc/Wallpapers/papes";
    };
  };
in
{
  imports = [
    ../../../nixos/fonts.nix
    ../../../desktop/jp.nix
    ../../../desktop/niri
  ];

  programs.firefox.enable = true;

  home-manager.users.jmc = {

    imports = [
      wallpaper
      obsidian
      pass
      ../../../desktop/niri/qalculate.nix
    ];

    home.packages = [
      unstable.spotify
      pkgs.tdesktop
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

    programs.waybar = {
      enable = false;
      systemd.enable = true;
      settings.mainBar = {
        height = 30;
        layer = "top";
        modules-left = [ "niri/workspaces" "niri/window" ];
        modules-center = [ ];
        modules-right = [ "memory" "cpu" "clock" "tray" ];
        spacing = 4;
        "niri/workspaces" = {
          format = "{value}";
        };
        "niri/window" = {
          icon = true;
          separate-outputs = true;
        };
        clock = {
          format = "{:%a %b %d %H:%M}";
          timezones = [ "Asia/Tokyo" "Europe/Amsterdam" ];
          tooltip-format = "{tz_list}";
        };
        cpu = {
          interval = 1;
          format = "{icon0}{icon1}{icon2}{icon3}{icon4}{icon5}{icon6}{icon7}{icon8}{icon9}{icon10}{icon11}";
          format-icons = [ "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" ];
        };
      };
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
