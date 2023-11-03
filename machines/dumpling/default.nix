{ config, lib, pkgs, ... }:

let
  ndhHosts = {
    networking.extraHosts = ''
      192.168.11.107	gitlab.ndh
      192.168.11.106	ndh106
    '';
    home-manager.users.jmc.programs.ssh.matchBlocks = {
      "gitlab.ndh" = {
        user = "jcarpay";
        identityFile = "~/Keys/ssh/id_ndh";
      };
      "ndh106" = {
        user = "jcarpay";
        identityFile = "~/Keys/ssh/id_ndh";
      };
    };
  };

  githubHosts = {
    home-manager.users.jmc.programs.ssh.matchBlocks."github.com" = {
      user = "git";
      identityFile = "~/Keys/ssh/id_ed25519";
    };
  };

  polybar = {
    imports = [ ../../desktop/polybar.nix ];
    services.polybar = {
      enable = true;
      settings."bar/mybar" = {
        "inherit" = "bar/common bar/hidpi";
        modules-right = "notifications vpn wireless wired fs memory cpu date-nl date";
      };
    };
  };

in
{
  imports = [
    ../../system/graphical.nix
    ../../system/global.nix
    ../../system/fonts.nix
    ndhHosts
    githubHosts
  ];

  home-manager.users.jmc.imports = [
    polybar
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "usbhid" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/8002582c-12c4-413e-8a55-89b836e7baa4";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/B859-3A65";
      fsType = "vfat";
    };

  swapDevices = [ ];
  programs.fish.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s5.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
  hardware.parallels.enable = true;
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "prl-tools" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "dumpling"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";

  i18n.defaultLocale = "en_US.UTF-8";

  networking.firewall.enable = false;

  services.xserver.enable = true;

  # Arch wiki says multiples of 96 work best
  # https://wiki.archlinux.org/title/HiDPI#X_Resources
  services.xserver.dpi = 96 * 2;

  # Enable sound with pipewire.
  # sound.enable = true;
  # hardware.pulseaudio.enable = false;
  # security.rtkit.enable = true;
  # services.pipewire = {
  #   enable = true;
  #   alsa.enable = true;
  #   alsa.support32Bit = true;
  #   pulse.enable = true;
  #   # If you want to use JACK applications, uncomment this
  #   #jack.enable = true;

  #   # use the example session manager (no others are packaged yet so this is enabled by default,
  #   # no need to redefine it in your config for now)
  #   #media-session.enable = true;
  # };

  home-manager.users.jmc = {
    home = {
      stateVersion = "23.05";
      sessionVariables.ST_FONT = "DM Mono Nerd Font:pixelsize=24:antialias=true:autohint=true";
    };
    programs.password-store.settings.PASSWORD_STORE_DIR = "/media/psf/Home/Passwords";
  };

  nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = "nix-command flakes";

  environment.systemPackages = with pkgs; [
    vim
  ];

  services.openssh.enable = true;

  system.stateVersion = "23.05";

}
