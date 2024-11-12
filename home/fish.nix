{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    shellInit = ''
      function gi
        curl -sL https://www.gitignore.io/api/$argv
      end

      function take -a dir
        if test -n "$dir"
          mkdir -p $dir
          and cd $dir
        end
      end

      set fish_greeting

      function fish_prompt
        ${pkgs.powerline-go}/bin/powerline-go \
          -error $status \
          -shell bare \
          -jobs (jobs -p | wc -l | xargs) \
          -hostname-only-if-ssh \
          -modules venv,host,ssh,cwd,perms,git,exit,nix-shell,jobs
      end

      function fzcd
          set -l dir (frecently view ~/.local/share/frecently/directory-history | fzf --no-sort --preview 'tree -C {} --gitignore -L 1')
          if test -n "$dir"
              cd $dir
              and commandline -f repaint
          end
      end
      bind \cs 'fzcd'

    '';
    shellAbbrs = {
      c = "cd";
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
      gpf = "git push --force";
      gpu = "git push --set-upstream origin (git rev-parse --abbrev-ref HEAD)";
      gra = "git rebase --abort";
      grc = "git rebase --continue";
      grh = "git reset HEAD --hard";
      gri = "git rebase -i";
      gs = "git status";
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
