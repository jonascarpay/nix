{ pkgs, ... }:

let

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
    ./hardware-configuration.nix
    ../../nixos/home-manager-xsession.nix
    ../../nixos/global.nix
    ../../nixos/fonts.nix
    ../../nixos/ndh.nix
    githubHosts
  ];

  home-manager.users.jmc.imports = [
    polybar
    ../../desktop
  ];

  programs.fish.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "dumpling"; # Define your hostname.

  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";

  i18n.defaultLocale = "en_US.UTF-8";

  networking.firewall.enable = false;

  services.xserver.enable = true;

  # Arch wiki says multiples of 96 work best
  # https://wiki.archlinux.org/title/HiDPI#X_Resources
  services.xserver.dpi = 96 * 2;

  home-manager.users.jmc = {
    home = {
      stateVersion = "23.05";
      sessionVariables.ST_FONT = "DM Mono Nerd Font:pixelsize=24:antialias=true:autohint=true";
    };
    programs.password-store.settings.PASSWORD_STORE_DIR = "/media/psf/Home/Passwords";
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
  ];

  services.openssh.enable = true;

  system.stateVersion = "23.05";

}
