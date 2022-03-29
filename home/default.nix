{ pkgs, unstable, config, ... }: {
  imports = [
    ./crypt.nix
    ./direnv.nix
    ./fish.nix
    ./ranger.nix
    ./scripts.nix
    ./tmux.nix
    ./vim
    ./zsh.nix
    ./git.nix
  ];

  manual.manpages.enable = true;

  home = {
    packages = with pkgs; [
      ag
      cloc
      entr
      killall
      manpages
      neofetch
      s-tui
      tldr
      unstable.youtube-dl
      weechat
    ];

    sessionVariables = {
      EDITOR = "nvim";
      PAGER = "less";
    };

    file = {
      stackConfig = {
        target = ".stack/config.yaml";
        text = ''
          templates:
            params:
              author-name: Jonas Carpay
              author-email: jonascarpay@gmail.com
              github-username: jonascarpay
        '';
      };
      ghciConf = {
        target = ".ghci";
        text = ''
          :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
          :set prompt-cont "\ESC[0;34m | \ESC[m"
          :set +m
          :def remain \_args -> pure $ unlines [":reload", ":main"]
        '';
      };
    };
  };

  programs = {
    man.generateCaches = true;
    fzf = {
      enable = true;
      package = unstable.fzf;
    };

    z-lua = {
      enable = true;
      options = [ "enhanced" "once" "fzf" ];
      enableAliases = true;
      # https://github.com/nix-community/home-manager/blob/master/modules/programs/z-lua.nix#L9
    };

    git = {
      enable = true;
      userName = "Jonas Carpay";
      userEmail = "jonascarpay@gmail.com";
      ignores = [
        "result"
        "result-*"
        # The reason we put synchting here, and not in a, say, desktop
        # syncthing module is that onigiri needs these during syncing even though syncthing is
        # a system module there.
        ".stversions"
        "*.sync-confict-*"
        ".stignore"
        ".stfolder"
      ];
      extraConfig = {
        commit.verbose = true;
        pull.rebase = true;
        init.defaultBranch = "master";
      };
    };
    ssh = {
      enable = true;
      matchBlocks."pichanaki.xc".user = "jonas";
      matchBlocks."anakin.xc".user = "jonas";
    };
  };
}
