{ pkgs, ... }:
{
  # programs.emacs.extraPackages = _: [ pkgs.emacs-all-the-icons-fonts ];

  programs.emacs.init.usePackage = {

    all-the-icons.enable = true;

    solaire-mode = {
      enable = true;
      hook = [ "(after-init . solaire-global-mode)" ];
    };

    doom-themes = {
      enable = true;
      config = ''
        (load-theme 'doom-one t)
        (doom-themes-neotree-config)
        (doom-themes-org-config)
      '';
      # ;; Enable flashing mode-line on errors
      # (doom-themes-visual-bell-config)
    };

    doom-modeline = {
      enable = true;
      init = ''
        (doom-modeline-mode 1)
        (setq doom-modeline-icon t) ; For some reason we need this for emacsclient, see manual
        (setq doom-modeline-height 25)
      '';
    };

    neotree.enable = true;
  };
}
