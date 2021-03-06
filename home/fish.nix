{ pkgs, config, ... }:
let
  powerline = pkgs.unstable.powerline-go.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "jonascarpay";
      repo = "powerline-go";
      rev = "fce0e2a528beac0491eb84dffedda6afc89e0963";
      sha256 = "1psr2b31faqi9ks6fzpbx80ldcdxww7afd7k4bgvkalsgws4r60y";
    };
  });
in
{
  programs.fish = {
    enable = true;
    shellInit = ''
      function gi
        curl -sL https://www.gitignore.io/api/$argv
      end

      function fish_prompt
        ${powerline}/bin/powerline-go \
          -error $status \
          -shell bare \
          -modules venv,ssh,cwd,perms,git,exit,nix-shell,jobs \
          -jobs (jobs -p | wc -l)
      end

    '';
    shellAbbrs = {
      c = "cd";
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
      nc = "nix-channel";
      ncl = "nix-channel --list";
      ncu = "nix-channel --update";
      ng = "nix-collect-garbage";
      nsp = "nix-shell -p";
      rn = "ranger";
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
      t = "cd (mktemp -d)";
      v = "vim";
      vd = "vimdir";
      vn = "vimdir ~/nix";
    };
  };
  home.file.".tmux.conf".text =
    "set-option -g default-command /etc/profiles/per-user/jmc/bin/fish";
}
