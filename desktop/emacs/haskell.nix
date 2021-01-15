{ pkgs, ... }:
{
  programs.emacs.init.modules = {
    haskell-mode.config =
      let
        remap-o = ''
          ;; workaround for indentation not working in 2020
          ;; https://github.com/haskell/haskell-mode/issues/1265
          ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/haskell/autoload.el
          ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/haskell/config.el
          (defun +haskell/evil-open-above ()
            "Opens a line above the current mode"
            (interactive)
            (evil-digit-argument-or-evil-beginning-of-line)
            (haskell-indentation-newline-and-indent)
            (evil-previous-line)
            (haskell-indentation-indent-line)
            (evil-append-line nil))
          (defun +haskell/evil-open-below ()
            "Opens a line below the current mode"
            (interactive)
            (evil-append-line nil)
            (haskell-indentation-newline-and-indent)
          )
          (general-define-key
            :states 'normal
            :keymaps 'haskell-mode-map
            "o" #'+haskell/evil-open-below
            "O" #'+haskell/evil-open-above
          )
        '';
      in
      ''
        (use-package haskell-mode
          :mode "\\.hs\\'"
          :after evil
          :config
            (require 'reformatter)
            (reformatter-define ormolu
              :program "ormolu"
              :lighter " OHF")
            (add-hook 'haskell-mode-hook 'ormolu-on-save-mode)
            ${remap-o}
            (defun haskell-setup ()
                "Setup variables for editing Haskell files."
                ;; (haskell-indentation-mode -1)
                ;; (add-hook 'haskell-mode-hook #'turn-off-evil-auto-indent)
                (make-local-variable 'tab-stop-list)
                (setq tab-stop-list (number-sequence 0 120 2))
                (setq indent-line-function 'tab-to-tab-stop)

                ; Backspace: delete spaces up until a tab stop
                (defvar my-offset 2 "My indentation offset. ")
                (defun backspace-whitespace-to-tab-stop ()
                    "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
                    (interactive)
                    (let ((movement (% (current-column) my-offset))
                            (p (point)))
                        (when (= movement 0) (setq movement my-offset))
                        ;; Account for edge case near beginning of buffer
                        (setq movement (min (- p 1) movement))
                        (save-match-data
                        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
                            (backward-delete-char (- (match-end 1) (match-beginning 1)))
                            (call-interactively 'backward-delete-char)))))
                (local-set-key (kbd "DEL") 'backspace-whitespace-to-tab-stop)
            )
            (add-hook 'haskell-mode-hook #'haskell-setup)
        )
      '';

    lsp-haskell.config = ''
      (use-package lsp-haskell
        :custom (lsp-haskell-server-path "haskell-language-server")
        :hook (haskell-mode . lsp)
        :hook (haskell-literate-mode-hook . lsp)
      )
    '';
  };
}
