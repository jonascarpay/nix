{ pkgs, inputs, unstable, ... }:
let

  home-manager = {
    imports = [ inputs.home-manager.darwinModules.home-manager ];
    home-manager = {
      # Currently necessary when using HM as a flake
      # https://nix-community.github.io/home-manager/index.html#sec-flakes-nixos-module
      # https://github.com/divnix/digga/issues/30#issuecomment-748530996
      # useGlobalPkgs = true;
      # useUserPackages = true;
      extraSpecialArgs = { inherit unstable inputs; };
      users.jmc = import ../../home;
    };
  };

  ndhHosts = {
    environment.etc.hosts.text = ''
      192.168.11.101	ndh101
      192.168.11.106	ndh106
      192.168.11.107	gitlab.ndh
      192.168.11.108	ndh108
    '';
    home-manager.users.jmc.programs.ssh.matchBlocks =
      let
        cfg = {
          user = "jcarpay";
          identityFile = "/Users/jmc/Keys/ssh/id_ndh";
        };
      in
      {
        "gitlab.ndh" = cfg;
        "ndh101" = cfg;
        "ndh106" = cfg;
        "ndh108" = cfg;
      };
  };

  # TODO move to module, repeated a lot
  githubHosts = {
    home-manager.users.jmc.programs.ssh.matchBlocks."github.com" = {
      user = "git";
      identityFile = "/Users/jmc/Keys/ssh/id_ed25519";
    };
  };

  pass = {
    home-manager.users.jmc = {
      programs.password-store = {
        enable = true;
        settings.PASSWORD_STORE_DIR = "/Users/jmc/Passwords";
      };
    };
  };
in
{
  imports = [
    ndhHosts
    githubHosts
    pass
    home-manager
  ];
  environment.systemPackages =
    [
      pkgs.ranger
    ];

  # Default /etc/hosts content
  environment.etc.hosts = {
    copy = true;
    text = ''
      127.0.0.1	localhost
      255.255.255.255	broadcasthost
      ::1             localhost
    '';
  };

  services.nix-daemon.enable = true;

  nix.settings.experimental-features = "nix-command flakes";

  programs.zsh.enable = true;

  # This is needed to make fish correctly source the nix environment variables.
  # Without this, making ~/.nix-profile/bin/fish the default shell in iTerm will
  # end up picking up the system's vim, for example
  programs.fish.enable = true;

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

  home-manager.users.jmc = {
    home.stateVersion = "23.05";
    # imports = [ ../../home ];
    home.packages = [ pkgs.mosh ];
  };

  users.users.jmc = {
    # description = "Jonas Carpay";
    home = "/Users/jmc";
    shell = pkgs.fish;
  };
}
