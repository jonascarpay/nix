{ pkgs, unstable, inputs, ... }:
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
      ./wallpaper.nix
      obsidian
    ];

    home.packages = [
      pkgs.wl-clipboard
      pkgs.spotify
      pkgs.tdesktop
      pkgs.okular
      unstable.signal-desktop
    ];

    programs.waybar = {
      enable = true;
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
