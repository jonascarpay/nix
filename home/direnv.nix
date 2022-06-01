{
  programs = {
    direnv = {
      enable = true;
      # enableBashIntegration = true;
      # enableFishIntegration = true;
      nix-direnv.enable = true;
    };
    git.ignores = [
      ".envrc"
      ".direnv"
    ];
    fish.shellAbbrs = {
      nde = "echo 'use nix' > .envrc; and direnv allow";
      ndf = "echo 'use flake' > .envrc; and direnv allow";
    };
    fish.shellInit = ''
      set -x DIRENV_LOG_FORMAT ""
    '';
  };
}
