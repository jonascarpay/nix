{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      silent = true;
    };
    git.ignores = [
      ".envrc"
      ".direnv"
    ];
    fish.shellAbbrs = {
      nde = "echo 'use nix' > .envrc; and direnv allow";
      ndf = "echo 'use flake . -L' > .envrc; and direnv allow";
    };
    fish.shellInit = ''
      set -x DIRENV_LOG_FORMAT ""
    '';
  };
}
