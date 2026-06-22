{ pkgs, inputs, config, ... }:
let
  freqle-pkg = inputs.freqle.packages.${pkgs.system}.default;

  freqle = "${freqle-pkg}/bin/freqle";

  freqle-dir-history = "${config.xdg.dataHome}/freqle/directory-history";
  freqle-cmd-history = "${config.xdg.dataHome}/freqle/command-history";

in
{
  home.packages = [ freqle-pkg ];

  programs.fish = {
    enable = true;
    functions = {
      gi = "curl -sL https://www.gitignore.io/api/$argv";
      take = {
        argumentNames = "dir";
        body = ''
          mkdir -p $dir
          and cd $dir
        '';
      };
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

      function __freqle-directory-hook --on-variable PWD --description 'add current directory to directory history'
        ${freqle} bump ${freqle-dir-history} "$PWD"
      end

      function fzcd
          set -l dir (${freqle} view ${freqle-dir-history} | fzf --no-sort --preview '${pkgs.lib.getExe pkgs.tree} -C {} --gitignore -L 1')
          if test -n "$dir"
              cd $dir
              and commandline -f repaint
          end
      end
      bind \cs 'fzcd'

      function __freqle_command_hook --on-event fish_postexec --description 'bump command in frecency history'
          set -l cmd (string trim -- $argv[1])
          test -n "$cmd"; or return
          string match -qr '\n' -- $cmd; and return
          ${freqle} bump ${freqle-cmd-history} -- $cmd &
          disown
      end

      function fzcmd --description 'frecency command picker'
          set -l cmd (${freqle} view ${freqle-cmd-history} | fzf --no-sort --query (commandline))
          if test -n "$cmd"
              commandline -r -- $cmd
          end
          commandline -f repaint
      end
      bind \co 'fzcmd'
    '';
    shellAbbrs = {
      c = "cd";
      cgr = "cd (git rev-parse --show-toplevel)";
      cwr = "cd (dirname (git rev-parse --git-common-dir))";
      dv = "cd ~/Dev/";
      ndh = "cd ~/Dev/NDH/";
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
      nsp = "nix shell nixpkgs#";
      ns = "nix shell";
      nd = "nix develop";
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
      ym = "date +%Y%m";
    };
  };
  home.file.".tmux.conf".text =
    "set-option -g default-command fish";
}
