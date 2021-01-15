{ pkgs, ... }:
{
  programs.emacs.init.modules = {

    undo-tree.config = ''
      (use-package undo-tree
        :config
        (global-undo-tree-mode)
      )
    '';
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
          (evil-set-undo-system 'undo-tree)
          ;; fixes not letting go of ctrl
          (general-define-key
            :states 'normal
            :prefix "C-w"
            "C-h" 'evil-window-left
            "C-l" 'evil-window-right
            "C-j" 'evil-window-down
            "C-k" 'evil-window-up
          )
        (defun turn-off-evil-auto-indent ()
            (setq-local evil-auto-indent nil))
        )
      '';
    };

    # From Justin's config
    # (setq-default indent-tabs-mode nil)
    # (setq-default tab-width 2)
    # (setq tab-width 2)
    # (setq-default evil-shift-width 2)
    # (setq evil-shift-width 2)
    # 
    # (superword-mode t)
    # 
    # (add-hook 'haskell-mode-hook #'turn-off-evil-auto-indent)
    # (add-hook 'purescript-mode-hook #'turn-off-evil-auto-indent)
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

    evil-commentary.config = ''
      (use-package evil-commentary
        :after evil
        :config  (evil-commentary-mode))
    '';

  };
}
