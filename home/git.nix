{ pkgs, ... }:
let
  delta = let deltaBin = "${pkgs.delta}/bin/delta"; in
    {
      programs.git.extraConfig = {
        pager = {
          diff = deltaBin;
          log = deltaBin;
          reflog = deltaBin;
          show = deltaBin;
        };
        delta = {
          navigate = true;
          line-numbers = true;
          side-by-side = false;
        };
        interactive.diffFilter = "${deltaBin} --color-only --features=interactive";
      };
    };
in
{
  imports = [ delta ];
  programs.git = {
    enable = true;
    userName = "Jonas Carpay";
    userEmail = "jonascarpay@gmail.com";
    ignores = [ ".stfolder" ".stversions" ".stignore" "result" "result-*" ];
    extraConfig = {
      commit.verbose = true;
      pull.rebase = true;
      init.defaultBranch = "master";
    };
  };
}
