{
  programs.firefox = {
    enable = true;
    # package =
    #   let
    #     pkgs = import
    #       (builtins.fetchGit {
    #         url = "https://github.com/NixOS/nixpkgs.git";
    #         rev = "3ba617416cd7eb7f2d1872301a653116a6e4c163";
    #       })
    #       { };
    #   in
    #   pkgs.firefox;
    # profiles.jmc = {
    #   isDefault = true;
    #   settings = { };
    #   userChrome = "";
    # };
  };
}
