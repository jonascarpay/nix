{ pkgs, config, ... }:
let
  unstable = config.channels.unstable;
  declCachix = builtins.fetchTarball {
    url = "https://github.com/jonascarpay/declarative-cachix/archive/2d37297b3aa1281193b1a3ca208c77467772cf5c.tar.gz";
    sha256 = "1lv4v367a17qq4wvmqy95s86g5ias08hx0lwf8r9mbgk61fmfb68";
  };
in
{
  imports = [
    ./channels.nix
    (import "${declCachix}/home-manager.nix")
    ./fish.nix
    ./ranger.nix
    ./scripts.nix
    ./vim
    ../secrets/xcjp-cache.nix
    ./tmux.nix
    ./direnv.nix
  ];

  caches = {
    cachix = [{
      name = "jmc";
      sha256 = "1bk08lvxi41ppvry72y1b9fi7bb6qvsw6bn1ifzsn46s3j0idq0a";
    }];
    extraCaches = [
      {
        url = "https://hydra.iohk.io";
        key = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
      }
    ];
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
      unzip
      weechat
      unstable.youtube-dl
      manpages
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

  nixpkgs.overlays = [
    (self: super: {
      fzf = unstable.fzf;
    })
  ];
  programs = {
    fzf.enable = true;

    home-manager.enable = true;

    git = {
      enable = true;
      userName = "Jonas Carpay";
      userEmail = "jonascarpay@gmail.com";
      ignores = [ "result" "result-*" ];
      extraConfig = {
        commit.verbose = true;
        init.defaultBranch = "master";
      };
    };
  };

}
