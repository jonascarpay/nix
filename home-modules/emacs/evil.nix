{
  programs.emacs.init.modules = {

    evil = {
      precedence = 2;
      config = ''
        (use-package evil
          :init
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)
          (setq evil-want-C-u-scroll t) ;; scroll with C-u
          :config
          (evil-mode 1)
          ;; also exists: https://github.com/rolandwalker/simpleclip
          (setq x-select-enable-clipboard nil)
        )
      '';
    };

    general = {
      precedence = 1;
      config = ''
        (use-package general
          :after evil
          :config
          (general-create-definer lmap
            :prefix "SPC"
            :keymaps 'normal)
          (lmap "w" 'save-buffer))
      '';
    };

    # https://github.com/syl20bnr/evil-escape
    # TODO use general for this?
    evil-escape.config = ''
      (use-package evil-escape
        :config
        (evil-escape-mode t)
        (setq-default evil-escape-key-sequence "hj")
      )
    '';

    # https://github.com/Somelauw/evil-org-mode
    evil-org.config = ''
      (use-package evil-org
        :after org
        :hook (org-mode . evil-org-mode)
        :config
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys))
    '';

    # https://github.com/emacs-evil/evil-collection
    evil-collection.config = ''
      (use-package evil-collection
        :after evil
        :config
        (evil-collection-init)
      )
    '';

    surround = {
      packages = [ "evil-surround" "embrace" "evil-embrace" ];
      config = ''
        (use-package evil-surround
          :config
          (global-evil-surround-mode 1))
        (use-package embrace)
        (use-package evil-embrace)
        (evil-embrace-enable-evil-surround-integration)
        (setq evil-embrace-show-help-p nil)
        (add-hook 'org-mode-hook 'embrace-org-mode-hook)
      '';
    };

    # https://github.com/abo-abo/avy
    # TODO: line jump markers at first character
    avy.config = ''
      (use-package avy
        :after general
        :config
        (general-define-key
          :states '(normal visual) ;; TODO 'motion?
          :prefix "SPC SPC"
          "f" 'evil-avy-goto-char-timer
          "j" 'evil-avy-goto-line
          "k" 'evil-avy-goto-line
          "w" 'evil-avy-goto-word-0
          "W" 'evil-avy-goto-word-0
          "b" 'evil-avy-goto-word-0
          "B" 'evil-avy-goto-word-0
        )
      )
    '';

  };
}
