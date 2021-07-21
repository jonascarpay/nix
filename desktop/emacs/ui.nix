{ pkgs, ... }:
{

  programs.emacs.init.modules = {

    all-the-icons.packages = [ pkgs.emacs-all-the-icons-fonts ];
    all-the-icons.config = ''
      (use-package all-the-icons)
    '';

    solaire-mode.config = ''
      ;; A more complex, more lazy-loaded config
      (use-package solaire-mode
        :config
        (solaire-global-mode +1)
      )
    '';

    doom-themes.config = ''
      (use-package doom-themes
        :after solaire-mode
        :config
        (load-theme 'doom-nord t)
      )
    '';

    doom-modeline.config = ''
      (use-package doom-modeline
        :after doom-themes
        :config
        (doom-modeline-mode 1)
        (setq doom-modeline-icon t) ; For some reason we need this for emacsclient, see manual
      )
    '';

    treemacs.config = ''
      (use-package treemacs
        :general
        (:states 'normal
          "C-n" 'treemacs)
        :config
        (setq treemacs-python-executable "${pkgs.python3}/bin/python")
        (treemacs-git-mode 'deferred)
        (require 'treemacs-all-the-icons)
        (treemacs-load-theme "all-the-icons")
      )
    '';
    treemacs-projectile.config = ''
      (use-package treemacs-projectile
        :after treemacs projectile)
    '';
    treemacs-magit.config = ''
      (use-package treemacs-magit
        :after treemacs magit)
    '';
    treemacs-evil.config = ''
      (use-package treemacs-evil
        :after treemacs evil)
    '';

    treemacs-all-the-icons.config = ''
      (use-package treemacs-all-the-icons
        :defer t)
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
          '((agenda . 10)
            ;; (projects . 10)
            (recents  . 20)
            ;; (bookmarks . 5)
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
