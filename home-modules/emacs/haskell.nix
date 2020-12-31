{
  programs.emacs.init.modules = {
    haskell-mode.config = ''
      (use-package haskell-mode
        :mode "\\.hs\\'"
        :config
          (require 'eglot)
          (require 'reformatter)
          (reformatter-define ormolu
            :program "ormolu"
            :lighter " OHF")
          (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server" "--lsp")))
          (add-hook 'haskell-mode-hook 'eglot-ensure)
          (add-hook 'haskell-mode-hook 'ormolu-on-save-mode)

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
      )
    '';
  };
}
