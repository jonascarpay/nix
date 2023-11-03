{ pkgs, ... }:
let
  ndhHosts = {
    environment.etc.hosts.text = ''
      192.168.11.107	gitlab.ndh
      192.168.11.106	ndh106
    '';
    home-manager.users.jmc.programs.ssh.matchBlocks = {
      "gitlab.ndh" = {
        user = "jcarpay";
        identityFile = "/Users/jmc/Keys/ssh/id_ndh";
      };
      "ndh106" = {
        user = "jcarpay";
        identityFile = "/Users/jmc/Keys/ssh/id_ndh";
      };
    };
  };

  githubHosts = {
    home-manager.users.jmc.programs.ssh.matchBlocks."github.com" = {
      user = "git";
      identityFile = "/Users/jmc/Keys/ssh/id_ed25519";
    };
  };

  pass = {
    home-manager.users.jmc.programs.password-store = {
      enable = true;
      settings.PASSWORD_STORE_DIR = "/Users/jmc/Passwords";
    };
  };
in
{
  imports = [
    ndhHosts
    githubHosts
    pass
  ];
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
      # pkgs.pass
      # pkgs.pinentry_mac
      # pkgs.qtpass
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

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  nix.settings.experimental-features = "nix-command flakes";

  programs.zsh.enable = true; # default shell on catalina

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
    imports = [ ../../home ];
  };

  users.users.jmc = {
    # description = "Jonas Carpay";
    home = "/Users/jmc";
    shell = pkgs.fish;
  };
}
