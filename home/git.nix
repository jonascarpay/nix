{ pkgs, ... }:
let
  difft = {
    programs.git.extraConfig = {
      # https://difftastic.wilfred.me.uk/git.html
      diff.tool = "difftastic";
      difftool.prompt = false;
      "difftool \"difftastic\"".cmd = "${pkgs.difftastic}/bin/difft \"$LOCAL\" \"$REMOTE\"";
    };
    programs.fish.shellAbbrs.gdt = "git difftool";
    programs.fish.shellAbbrs.gdd = "git difftool";
  };
in
{
  imports = [ difft ];
  programs.git = {
    diff-so-fancy.enable = true;
    enable = true;
    userName = "Jonas Carpay";
    userEmail = "jonascarpay@gmail.com";
    ignores = [ ".stfolder" ".stversions" ".stignore" "result" "result-*" ];
    aliases = {
      graph = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)' --all";
      # list and pretty-print branches by latest commit, from https://stackoverflow.com/questions/5188320/how-can-i-get-a-list-of-git-branches-ordered-by-most-recent-commit
      recentb = ''
        !r() {
          refbranch=$1 count=$2;
          git for-each-ref --sort=-committerdate refs/heads --format='%(refname:short)|%(HEAD)%(color:yellow)%(refname:short)|%(color:bold green)%(committerdate:relative)|%(color:blue)%(subject)|%(color:magenta)%(authorname)%(color:reset)' --color=always --count=''${count:-20} | while read line; do
            branch=$(echo "$line" | awk 'BEGIN { FS = "|" }; { print $1 }' | tr -d '*');
            ahead=$(git rev-list --count "''${refbranch:-origin/master}..''${branch}");
            behind=$(git rev-list --count "''${branch}..''${refbranch:-origin/master}");
            colorline=$(echo "$line" | sed 's/^[^|]*|//');
            echo "+$ahead|-$behind|$colorline" | awk -F'|' -vOFS='|' '{$5=substr($5,1,70)}1';
          done | column -ts'|';
        }; r
      '';
    };
    extraConfig = {
      commit.verbose = true;
      pull.rebase = true;
      init.defaultBranch = "master";
    };
  };
}
