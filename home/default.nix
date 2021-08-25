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
    fzf = {
      enable = true;
      package = unstable.fzf;
    };

  };

}
