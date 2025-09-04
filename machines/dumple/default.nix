{ pkgs, lib, inputs, config, ... }:

let

  wallpaper = {
    imports = [ ../../desktop/random-wallpaper.nix ];
    services.randomWallpaper = {
      enable = true;
      wallpaperPath = "${inputs.wallpapers}/papes";
    };
  };

  netrc = {
    nix.settings.netrc-file = config.age.secrets.netrc.path;
    age.secrets.netrc.file = ../../secrets/woven/netrc.age;
  };

  personal-github = {
    age.secrets.id_personal_github = {
      file = ../../secrets/woven/id_personal_github.age;
      owner = "jmc";
    };
    home-manager.users.jmc.programs.ssh.matchBlocks.github_personal = {
      hostname = "github.com";
      user = "git";
      identityFile = config.age.secrets.id_personal_github.path;
    };
  };

in
{
  imports = [
    ./hardware-configuration.nix
    ../../nixos/global.nix
    ../../nixos/fonts.nix
    ../../desktop
    netrc
    personal-github
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "woven";

  networking.networkmanager.enable = true;
  services.timesyncd.enable = lib.mkForce true;
  services.automatic-timezoned.enable = true;

  i18n.defaultLocale = "en_US.UTF-8"; # TODO to global

  networking.firewall.enable = false;

  services.xserver.enable = true;

  home-manager.users.jmc = {
    imports = [
      wallpaper
      ../../desktop/qalculate.nix
    ];

    home.stateVersion = "23.05";

    home.sessionPath = [ "$HOME/.cargo/bin" ];

    programs.neovide.settings.font.size = 10.0;
    programs.alacritty.settings.font.size = 10;
    programs.niri.settings = {
      outputs."Virtual-1".scale = 2;
      layout = {
        gaps = 16;
        struts.left = 32;
        struts.right = 32;
      };
      # window-rules = [
      #   {
      #     matches = [{ app-id = "Alacritty"; }];
      #     default-column-width.proportion = 0.25;
      #   }
      #   {
      #     matches = [{ app-id = "neovide"; }];
      #     default-column-width.proportion = 0.5;
      #   }
      # ];
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
    };

    programs.firefox.enable = true;

  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = [ pkgs.vim ];

  services.openssh.enable = true;

  system.stateVersion = "23.05";

  users.users.jmc.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfujv3vIl7EeRvjUyJyBZpFTSU6DguSYlJpSXzD7H7X Woven_key"
  ];
}
