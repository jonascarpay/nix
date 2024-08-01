{ pkgs, inputs, ... }: {
  imports = [
    ./direnv.nix
    ./fish.nix
    # ./ranger.nix
    # ./scripts.nix
    ./tmux.nix
    ./vim
    ./git.nix
    ./postgres.nix
    inputs.agenix.homeManagerModules.age
  ];

  manual.manpages.enable = true;

  home = {
    stateVersion = "23.05";
    packages = [
      pkgs.silver-searcher # TODO do I use this? yes for fzf fg
      pkgs.visidata
      pkgs.cloc
      pkgs.entr
      pkgs.jq
      # killall
      # man-pages # TODO does this do anything manual.manpages.enable doesn't do?
      pkgs.tldr
      pkgs.ripgrep
      # youtube-dl
      # weechat
      pkgs.zip
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
    nix-index.enable = true;
    man.generateCaches = true;
    fzf.enable = true;

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
        ".lvimrc"
      ];
      extraConfig = {
        commit.verbose = true;
        pull.rebase = true;
        init.defaultBranch = "master";
      };
    };
    ssh = {
      enable = true;
    };
  };
}
