{ pkgs, config, ... }:
let
  powerline-go = ''
    function fish_prompt
      ${pkgs.powerline-go}/bin/powerline-go \
        -error $status \
        -shell bare \
        -ignore-repos /home/jmc \
        -modules venv,ssh,cwd,perms,git,exit,nix-shell,jobs,root
    end
  '';

in
{
  programs.fish = {
    enable = true;
    shellInit = ''
      function fish_user_key_bindings
        ${if config.programs.fzf.enable then "fzf_key_bindings" else ""}
      end

      function gi
        curl -sL https://www.gitignore.io/api/$argv
      end

      ${powerline-go}

    '';
    shellAbbrs = {
      c = "cd";
      dc = "cd ~/Documents/";
      dn = "cd ~/Downloads/";
      dv = "cd ~/Dev/";
      ft = "rm tags; and fast-tags -Rv .";
      g = "git";
      ga = "git add";
      gaa = "git add --all";
      gap = "git add --patch";
      gb = "git branch";
      gbd = "git branch -d";
      gc = "git commit";
      gca = "git commit --amend";
      gcm = "git checkout master";
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
      hm = "home-manager";
      hms = "home-manager switch";
      l = "ls";
      ll = "ls -lah";
      n = "vim";
      nb = "nix build";
      nbl = "nix build -L";
      nbo = ''nix build -L --option substituters ""'';
      nc = "nix-channel";
      ncl = "nix-channel --list";
      ncu = "nix-channel --update";
      ne = "nix-env";
      ng = "nix-collect-garbage";
      ns = "cached-nix-shell";
      nsf = "cached-nix-shell --run fish";
      nsp = "nix-shell -p";
      nsr = "cached-nix-shell --run";
      nsv = "cached-nix-shell --run vim";
      nse = "cached-nix-shell --run emacs";
      rf = "rm -rf";
      rn = "ranger";
      s = "sudo";
      sb = "stack build";
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
      sh = "stack haddock; and stack hoogle --no-nix-pure --server";
      sha = "stack haddock --open --no-nix-pure";
      snc = "sudo nix-channel";
      sno = "sudo nixos-rebuild";
      sr = "stack repl";
      st = "stack";
      sv = "sudo vim";
      t = "tmux";
      v = "vim";
      vd = "vimdir";
      vn = "vimdir ~/nix";
    };
  };
  home.file.".tmux.conf".text =
    "set-option -g default-command /home/jmc/.nix-profile/bin/fish";


}

# vim: sw=2:ts=2:fdm=indent:foldcolumn=3:foldlevel=1
