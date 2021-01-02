{ pkgs, ... }:
{

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
  ];
  programs.emacs.init.modules = {

    all-the-icons.config = ''
      (use-package all-the-icons)
    '';

    solaire-mode.config = ''
      ;; A more complex, more lazy-loaded config
      (use-package solaire-mode
        :after doom-themes
        ;; Ensure solaire-mode is running in all solaire-mode buffers
        :hook (change-major-mode . turn-on-solaire-mode)
        ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
        ;; itself off every time Emacs reverts the file
        :hook (after-revert . turn-on-solaire-mode)
        ;; To enable solaire-mode unconditionally for certain modes:
        :hook (ediff-prepare-buffer . solaire-mode)
        ;; Highlight the minibuffer when it is activated:
        :hook (minibuffer-setup . solaire-mode-in-minibuffer)
        :config
        ;; The bright and dark background colors are automatically swapped the first 
        ;; time solaire-mode is activated. Namely, the backgrounds of the `default` and
        ;; `solaire-default-face` faces are swapped. This is done because the colors 
        ;; are usually the wrong way around. If you don't want this, you can disable it:
        (setq solaire-mode-auto-swap-bg nil)

        (solaire-global-mode +1)
        (load-theme 'doom-nord t)
      )
    '';

    doom-themes.config = ''
      (use-package doom-themes)
    '';

    doom-modeline.config = ''
      (use-package doom-modeline
        :after doom-themes
        :config
        (doom-modeline-mode 1)
        (setq doom-modeline-icon t) ; For some reason we need this for emacsclient, see manual
      )
    '';

    # TODO treemacs seems better
    neotree.config = ''
      (use-package neotree
        :general
        (:states 'normal
          "C-n" 'neotree-projectile-action
        )
        :config
        (setq neo-theme 'icons)
        (setq neo-window-fixed-size nil)
        (setq neo-reset-size-on-open nil) ;; TODO this doesn't work?
        ;; TODO don't move cursor
      )
    '';

    dashboard.config = ''
      (use-package dashboard
        :after all-the-icons general
        :init
        (setq dashboard-set-heading-icons t)
        (setq dashboard-set-file-icons t)
        (setq dashboard-startup-banner 'logo)
        (setq dashboard-center-content t)
        (setq dashboard-show-shortcuts t)
        (setq dashboard-page-separator "\n \n") ;; That's not a normal space
        (setq dashboard-items
          '((agenda . 5)
            (projects . 10)
            (recents  . 10)
            (bookmarks . 5)
            ))
        :config
        (dashboard-setup-startup-hook)
        (general-define-key
          :keymaps 'dashboard-mode-map
          :states 'normal
          "f" 'helm-projectile
          "c" 'org-capture
          "a" 'org-agenda
          "SPC" 'helm-projectile)
        (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
      )
    '';
  };

}
