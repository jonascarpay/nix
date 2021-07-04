{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      dnsutils
      exfat
      file
      git
      htop
      gotop
      ncdu
      ranger
      sshfs
      tree
      tmux
      vim
    ];
    variables = {
      EDITOR = "vim";
      PAGER = "less";
    };
    homeBinInPath = true;
  };

  system.copySystemConfiguration = true;
  nixpkgs.config.allowUnfree = true;
  security.sudo.wheelNeedsPassword = false;
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  programs.bash.enableCompletion = true; # enable tab-completion for nix-* tools

  users = {
    mutableUsers = false;
    users.jmc = {
      isNormalUser = true;
      description = "Jonas Carpay";
      createHome = true;
      extraGroups = [ "wheel" "networkmanager" "audio" ];
      openssh.authorizedKeys.keyFiles = [ ../../.ssh/id_rsa.pub ];
      hashedPassword = "$1$Nh9Zc.7Y$Zw9...mvYbA0qWE/PXKm7.";
    };
  };

  nix.trustedUsers = [ "root" "jmc" ];

}
