{ pkgs, config, ... }:
let
  unstable = config.channels.unstable;
  powerline = unstable.powerline-go.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "jonascarpay";
      repo = "powerline-go";
      rev = "fce0e2a528beac0491eb84dffedda6afc89e0963";
      sha256 = "1psr2b31faqi9ks6fzpbx80ldcdxww7afd7k4bgvkalsgws4r60y";
    };
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
      ng = "nix-collect-garbage";
      sng = "sudo nix-collect-garbage";
      nsp = "nix-shell -p";
      rn = "ranger";
      s = "sudo";
      sc = "systemctl --user";
      sca = "systemctl --user start";
      sco = "systemctl --user stop";
      snr = "sudo nixos-rebuild";
      snrs = "sudo nixos-rebuild switch";
      snrb = "sudo nixos-rebuild boot";
      snrbu = "sudo nixos-rebuild boot --upgrade";
      scr = "systemctl --user restart";
      scu = "systemctl --user status";
      ssc = "sudo systemctl";
      ssca = "sudo systemctl start";
      ssco = "sudo systemctl stop";
      sscr = "sudo systemctl restart";
      sscu = "sudo systemctl status";
      t = "tmux";
      v = "vim";
      vd = "vimdir";
      vn = "vimdir ~/nix";
    };
  };
  home.file.".tmux.conf".text =
    "set-option -g default-command /home/jmc/.nix-profile/bin/fish";

}
