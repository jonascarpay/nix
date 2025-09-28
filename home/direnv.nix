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
    fish.shellAbbrs.ndf = "echo 'use flake . -L' > .envrc; and direnv allow";
  };
}
