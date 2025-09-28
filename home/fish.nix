{ pkgs, inputs, ... }:
let
  frecently-pkg = inputs.frecently.defaultPackage.${pkgs.system};
in
{
  programs.fish = {
    enable = true;
    functions = {
      gi = "curl -sL https://www.gitignore.io/api/$argv";
      take = ''
        function take -a dir
          if test -n "$dir"
            mkdir -p $dir
            and cd $dir
          end
        end
      '';
    };
    shellInit = /* fish */ ''
      set fish_greeting

      function __update_git_root --on-variable PWD --description "set git root"
        if git root &>/dev/null
          set -g gr (git root)
        else
          set -ge gr
        end
      end
      __update_git_root

      function fzcd
          set -l dir (${frecently-pkg}/bin/frecently view ~/.local/share/frecently/directory-history | fzf --no-sort --preview 'tree -C {} --gitignore -L 1')
          if test -n "$dir"
              cd $dir
              and commandline -f repaint
          end
      end
      bind \cs 'fzcd'
    '';
    shellAbbrs = {
      c = "cd";
      cgr = "cd $gr";
      dv = "cd ~/Dev/";
      ndh = "cd ~/Dev/NDH/";
      dr = "direnv reload";
      g = "git";
      ga = "git add";
      gaa = "git add --all";
      gap = "git add --patch";
      gau = "git add --update";
      gb = "git branch";
      gbd = "git branch -d";
      gc = "git commit";
      gca = "git commit --amend";
      gco = "git checkout";
      gd = "git diff";
      ge = "git ls-files | entr -dcs";
      gl = "git pull";
      gp = "git push";
      gpf = "git push --force-with-lease";
      gpu = "git push --set-upstream origin (git rev-parse --abbrev-ref HEAD)";
      gr = "git root";
      gra = "git rebase --abort";
      grc = "git rebase --continue";
      grh = "git reset HEAD --hard";
      gri = "git rebase -i";
      gs = "git status";
      gw = "git worktree";
      gwa = "git worktree add";
      gwl = "git worktree list";
      l = "ls";
      ll = "ls -lah";
      n = "vim";
      ng = "nix-collect-garbage";
      nsp = "nix shell nixpkgs#";
      ns = "nix shell";
      nd = "nix develop";
      rn = "ranger";
      nn = "nnn";
      s = "sudo";
      sc = "systemctl --user";
      sca = "systemctl --user start";
      sco = "systemctl --user stop";
      scr = "systemctl --user restart";
      scu = "systemctl --user status";
      ssc = "sudo systemctl";
      ssca = "sudo systemctl start";
      ssco = "sudo systemctl stop";
      sscr = "sudo systemctl restart";
      sscu = "sudo systemctl status";
      tmp = "take /tmp/scratch";
      v = "vim";
    };
  };
  home.file.".tmux.conf".text =
    "set-option -g default-command fish";
}
