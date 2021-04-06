{
  programs.direnv.enable = true;
  programs.direnv.enableNixDirenvIntegration = true;
  programs.git.ignores = [
    ".envrc"
    ".direnv"
  ];
  programs.fish.shellAbbrs = {
    nde = "echo 'use nix' > .envrc; and direnv allow";
  };
  programs.fish.shellInit = ''
    set -x DIRENV_LOG_FORMAT ""
  '';
}
