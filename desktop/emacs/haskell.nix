{ pkgs, ... }:
let
  my-lsp-haskell = pkgs.emacsPackages.lsp-haskell.overrideAttrs
    (_: {
      src = pkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-haskell";
        rev = "592e883026288677f4042333b51efc7a40b9a68a";
        sha256 = "1dar5iapnx55z8875sr05p4b9nklmqdx6vfxhisy2hsgkp8iq8fc";
      };
    });
in
{
  programs.emacs.init.modules = {
    haskell-mode.config = ''
      (use-package haskell-mode
        :mode "\\.hs\\'"
        :config
          (require 'reformatter)
          (reformatter-define ormolu
            :program "ormolu"
            :lighter " OHF")
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

    lsp-haskell.packages = [ my-lsp-haskell ];
    lsp-haskell.config = ''
      (use-package lsp-haskell
        :custom (lsp-haskell-server-path "haskell-language-server")
        :hook (haskell-mode . lsp)
        :hook (haskell-literate-mode-hook . lsp)
      )
    '';
  };
}
