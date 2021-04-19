{ pkgs, config, ... }:
let
  unstable = config.channels.unstable;
  powerline = unstable.powerline-go.overrideAttrs (old: {
    patches = [ ./powerline-go.diff ];
  });
  powerline-fish = ''
    function fish_prompt
      ${powerline}/bin/powerline-go \
        -error $status \
        -shell bare \
        -modules venv,ssh,cwd,perms,git,exit,nix-shell,jobs \
        -jobs (jobs -p | wc -l)
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

      ${powerline-fish}

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
      nsp = "nix-shell -p";
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
      t = "tmux";
      v = "vim";
      vd = "vimdir";
      vn = "vimdir ~/nix";
    };
  };
  home.file.".tmux.conf".text =
    "set-option -g default-command /home/jmc/.nix-profile/bin/fish";

}
