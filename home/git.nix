{ pkgs, ... }:
let
  grove-clone = pkgs.writeShellScriptBin "grove-clone" (builtins.readFile ./grove-clone.sh);
in
{
  home.packages = [ grove-clone ];
  programs.git-worktree-switcher.enable = true;
  programs.git = {
    # diff-so-fancy.enable = true;
    difftastic = {
      enable = true;
      enableAsDifftool = true;
    };
    enable = true;
    lfs.enable = true;
    userName = "Jonas Carpay";
    userEmail = "jonascarpay@gmail.com";
    ignores = [
      # Don't know
      ".lvimrc"

      # Syncthing stuff
      ".stfolder"
      ".stversions"
      ".stignore"
      "*.sync-confict-*"

      # Nix stuff
      "result"
      "result-*"
    ];
    aliases = {
      graph = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)' --all";
      root = "rev-parse --show-toplevel";
      exec = "!exec ";
    };
    # https://blog.gitbutler.com/how-git-core-devs-configure-git
    extraConfig = {
      commit.verbose = true; # include diff in commit screen
      pull.rebase = true;
      init.defaultBranch = "master";
      merge.conflictStyle = "zdiff3";
      status.relativePaths = false;
      column.ui = "auto"; # list things (branches) in columns
      branch.sort = "-committerdate"; # sort branches reverse chronologically
      tag.sort = "version:refname"; # sensible version sorting
      diff = {
        algorithm = "histogram"; # just better diff algorithm
        renames = true; # detect renames
        mnemonicPrefix = true; # replace a/ and b/ in diffs with i/ (index) w/ (working dir) and c/ (commit)
      };
      push = {
        autoSetupRemote = true; # no more --set-upstream
        followTags = true; # automatically push tags
      };
      fetch = {
        # Gollow remote as closely as possible, including deletes.
        prune = true;
        pruneTags = true;
        all = true;
      };
      rerere = {
        # rerere means reuse recorded resolution
        # Reapply resolutions in rebases with conflicts
        enabled = true;
        autoupdate = true;
      };
      rebase = {
        # https://www.youtube.com/watch?v=Md44rcw13k4
        autoSquash = true;
        autoStash = true;
        updateRefs = true;
      };
      gc.worktreePruneExpire = "now";
      # worktree.useRelativePaths = true; # doesn't work :( https://github.com/libgit2/libgit2/issues/7099
    };
  };
}
