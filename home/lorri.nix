{ pkgs, ... }:
{
  services.lorri.enable = true;
  programs.direnv.enable = true;
  programs.git.ignores = [
    ".envrc"
  ];
  programs.fish.shellInit = ''
    set -x DIRENV_LOG_FORMAT ""
  '';
}
