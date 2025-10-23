{ pkgs, unstable, lib, inputs, config, ... }:

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
    age.secrets.netrc = {
      file = ../../secrets/woven/netrc.age;
      mode = "444";
    };
  };

  github-pat = {
    age.secrets.github-pat-nix-conf = {
      file = ../../secrets/woven/github_pat_nix_conf.age;
      mode = "444";
    };
    # TODO there might be some way to do this using nix.settings
    nix.extraOptions = ''
      !include ${config.age.secrets.github-pat-nix-conf.path}
    '';
  };

  personal-github = {
    age.secrets.id_personal_github = {
      file = ../../secrets/woven/id_personal_github.age;
      owner = "jmc";
    };
    home-manager.users.jmc.programs.ssh.matchBlocks = {
      floorbox = {
        hostname = "IN-197182981927-L.pc.internal.woven.tech";
        user = "jonas-carpay";
        identityFile = "~/.ssh/id_ed25519";
      };
      github_personal = {
        hostname = "github.com";
        user = "git";
        identityFile = config.age.secrets.id_personal_github.path;
      };
    };
  };

  claude = {
    home.packages = [
      unstable.claude-code
    ];
    programs.git.ignores = [ ".claude" ];
  };

in
{
  imports = [
    ./hardware-configuration.nix
    ../../nixos/global.nix
    ../../nixos/fonts.nix
    ../../desktop
    netrc
    github-pat
    personal-github
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  swapDevices = [{
    device = "/var/lib/swapfile";
    size = 32 * 1024;
  }];

  networking.hostName = "dumple";

  networking.networkmanager.enable = true;
  services.timesyncd.enable = lib.mkForce true;
  services.automatic-timezoned.enable = true;

  i18n.defaultLocale = "en_US.UTF-8"; # TODO to global

  networking.firewall.enable = false;

  hardware.opengl.enable = true;

  nix.settings.extra-experimental-features = [ "fetch-closure" ];

  services.xserver.enable = true;
  services.libinput.mouse.accelProfile = if config.services.libinput.enable then "flat" else builtins.abort "No libinput";
  services.libinput.touchpad.accelProfile = if config.services.libinput.enable then "flat" else builtins.abort "No libinput";

  home-manager.users.jmc = {
    imports = [
      wallpaper
      claude
      ../../desktop/qalculate.nix
    ];

    home.stateVersion = "23.05";

    home.sessionPath = [ "$HOME/.cargo/bin" ];

    programs.powerline-go.pathAliases."\\~/Dev/thirteenth-floor" = "13f";

    programs.neovide.settings.font.size = 10.0;
    programs.alacritty.settings.font.size = 10;
    programs.niri.settings = {
      outputs."Virtual-1".scale = 2;
      layout = {
        gaps = 16;
        struts.left = 32;
        struts.right = 32;
      };
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

  hardware.parallels.package = unstable.linuxPackages.prl-tools;

  users.users.jmc.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfujv3vIl7EeRvjUyJyBZpFTSU6DguSYlJpSXzD7H7X Woven_key"
  ];
}
