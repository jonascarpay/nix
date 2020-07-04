{ pkgs, config, lib, ... }:

let nurNoPkgs = import <nur> {pkgs = null;};
in {
  imports = [ nurNoPkgs.repos.rycee.hmModules.emacs-init ];
  programs.emacs.enable = true;

  programs.emacs.init = {
    enable = true;
    recommendedGcSettings = true;

    prelude = ''
      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (scroll-bar-mode 0)
      (toggle-frame-fullscreen)
      (setq inhibit-startup-message t)
      (setq initial-startup-message "")
    '';

    usePackage = {

      evil = {
        enable = true;
        config = ''
          (evil-mode)
        '';
      };

      evil-escape = {
        enable = true;
        config = ''
          (evil-escape-mode)
          (setq-default evil-escape-key-sequence "hj")
          (setq-default evil-escape-delay 0.4)
        '';
      };

      evil-numbers = {
        enable = true;
      };

      which-key = {
        enable = true;
        config = ''
          (which-key-mode)
          (which-key-setup-side-window-right-bottom)
        '';
      };

    };

  };

}
