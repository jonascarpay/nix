{ pkgs, ... }:
let
  delta = let fancyBin = "${pkgs.diff-so-fancy}/bin/diff-so-fancy"; in
    {
      programs.git.extraConfig = {
        core.pager = "${fancyBin} | less --tabs=4 -RFX;";
        interactive.diffFilter = "${fancyBin} --patch";
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
