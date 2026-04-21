{ inputs, pkgs, ... }:
{
  nixpkgs.overlays = [ inputs.jj-starship.overlays.default ];
  home.packages = [ inputs.jj-starship.packages.${pkgs.system}.jj-starship ];
  programs.starship = {
    enable = true;
    settings = {
      custom.jj = {
        when = "jj-staship detect";
        shell = [ "jj-starship" ];
        format = "$output ";
      };
      git_branch.disabled = true;
      git_status.disabled = true;
    };
  };
}
