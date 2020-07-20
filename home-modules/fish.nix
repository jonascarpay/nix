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

  powerline-fish = let
    fish-powerline-pkg = fetchGit {
      name = "theme-bobthefish";
      url = "https://github.com/oh-my-fish/theme-bobthefish";
    };
  in ''
    source ${fish-powerline-pkg}/functions/__bobthefish_glyphs.fish
    source ${fish-powerline-pkg}/functions/__bobthefish_colors.fish
    source ${fish-powerline-pkg}/fish_prompt.fish
  '';

in {
  programs.fish = {
    enable = true;
    shellInit = ''
      function fish_user_key_bindings
        ${if config.programs.fzf.enable then "fzf_key_bindings" else ""}
      end

      set fish_greeting

      function gi
        curl -sL https://www.gitignore.io/api/$argv
      end

      ${powerline-fish}

      source ~/.cache/wal/colors.fish
      set -g theme_color_scheme terminal

      cat ~/.cache/wal/sequences &
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
      rf = "rm -rf";
      rn = "ranger";
      s = "sudo";
      sb = "stack build";
      snc = "sudo nix-channel";
      sno = "sudo nixos-rebuild";
      sh = "stack haddock; and stack hoogle --no-nix-pure --server";
      sha = "stack haddock --open --no-nix-pure";
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

  home.file.".config/wal/templates/colors.fish".text = ''
    # set fish_color_normal normal
    set fish_color_normal {foreground.strip}
    # set fish_color_command 005fd7
    set fish_color_command {color2.strip}
    # set fish_color_param 00afff
    set fish_color_param {color1.strip}
    # set fish_color_redirection 00afff
    set fish_color_redirection $fish_color_param
    # set fish_color_comment 990000
    set fish_color_comment {color8.strip}
    set fish_color_error ff0000
    # set fish_color_escape 00a6b2
    set fish_color_escape {color5.strip}
    # set fish_color_operator 00a6b2
    set fish_color_operator $fish_color_escape
    set fish_color_end {color4.strip}
    set fish_color_quote {color6.strip}
    set fish_color_autosuggestion 555 brblack
    set fish_color_user brgreen
    # set fish_color_host normal
    set fish_color_host $fish_color_normal
    set fish_color_valid_path --underline
    set fish_color_cwd green
    set fish_color_cwd_root red
    set fish_color_match --background=brblue
    set fish_color_search_match bryellow --background=brblack
    set fish_color_selection white --bold --background=brblack
    set fish_color_cancel -r
    set fish_pager_color_prefix white --bold --underline
    set fish_pager_color_completion
    # set fish_pager_color_description B3A06D yellow
    set fish_pager_color_description $fish_color_quote yellow
    set fish_pager_color_progress brwhite --background=cyan
    set fish_color_history_current --bold
  '';

}

# vim: sw=2:ts=2:fdm=indent:foldcolumn=3:foldlevel=1
