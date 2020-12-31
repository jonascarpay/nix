{ pkgs, ... }:
let
  unstable = import <unstable> { };
in
{
  imports = [
    ./caches.nix
    ./emacs
    ./fish.nix
    ./ranger.nix
    ./scripts.nix
    ./vim
    ./tmux.nix
  ];

  caches = {
    cachix = [{
      name = "jmc";
      sha256 = "1bk08lvxi41ppvry72y1b9fi7bb6qvsw6bn1ifzsn46s3j0idq0a";
    }];
    extraCaches = {
      iohk-hydra = {
        url = "https://hydra.iohk.io";
        key = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
      };
    };
  };

  manual.manpages.enable = true;

  home = {
    packages = with pkgs; [
      ag
      entr
      killall
      neofetch
      s-tui
      tealdeer
      unstable.cached-nix-shell
      unzip
      weechat
      unstable.youtube-dl
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
    fzf.enable = true;

    home-manager.enable = true;

    git = {
      enable = true;
      userName = "Jonas Carpay";
      userEmail = "jonascarpay@gmail.com";
      ignores = [ "result" "result-*" ];
      extraConfig.commit.verbose = true;
    };
  };

}