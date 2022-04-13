{ pkgs, unstable, config, ... }:
{
  programs.fish = {
    enable = true;
    shellInit = ''
      function gi
        curl -sL https://www.gitignore.io/api/$argv
      end

      function take -a dir
        mkdir -p $dir
        cd $dir
      end

      function fish_prompt
        ${pkgs.powerline-go}/bin/powerline-go \
          -error $status \
          -shell bare \
          -modules venv,ssh,cwd,perms,git,exit,nix-shell,jobs \
          -jobs (jobs -p | wc -l)
      end

    '';
    shellAbbrs = {
      c = "cd";
      cn = "cd ~/nix/";
      dv = "cd ~/Dev/";
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
      gr = "git remote";
      gra = "git rebase --abort";
      grc = "git rebase --continue";
      grh = "git reset HEAD --hard";
      gri = "git rebase -i";
      gs = "git status";
      l = "ls";
      ll = "ls -lah";
      n = "vim";
      nb = "nix build";
      nbl = "nix build -L";
      nbo = ''nix build -L --option substituters ""'';
      ng = "nix-collect-garbage";
      nsp = "nix shell nixpkgs#";
      ns = "nix shell";
      nd = "nix develop";
      rn = "nnn";
      nn = "nnn";
      s = "sudo";
      sc = "systemctl --user";
      sca = "systemctl --user start";
      sco = "systemctl --user stop";
      scr = "systemctl --user restart";
      scu = "systemctl --user status";
      sng = "sudo nix-collect-garbage";
      snr = "sudo nixos-rebuild";
      snrb = "sudo nixos-rebuild boot";
      snrbu = "sudo nixos-rebuild boot --upgrade";
      snrs = "sudo nixos-rebuild switch";
      ssc = "sudo systemctl";
      ssca = "sudo systemctl start";
      ssco = "sudo systemctl stop";
      sscr = "sudo systemctl restart";
      sscu = "sudo systemctl status";
      tmp = "cd (mktemp -d)";
      tm = "tmux";
      tk = "take";
      v = "vim";
      vd = "vimdir";
      vn = "vimdir ~/nix";
    };
  };
  home.file.".tmux.conf".text =
    "set-option -g default-command /etc/profiles/per-user/jmc/bin/fish";
}
