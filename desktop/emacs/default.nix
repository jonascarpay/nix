{ pkgs, ... }: {
  imports = [
    ./evil.nix
    ./haskell.nix
    ./init.nix
    ./org.nix
    ./proof-general.nix
    ./ui.nix
  ];
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      # 21/01/04
      url = https://github.com/nix-community/emacs-overlay/archive/65e4ae8ddec6c94263aefb4ec9d8294b600280ae.tar.gz;
      sha256 = "1m434qh28ii1pp0xjnp3wqy009z2pn3km36iffr87d1sk4122f2h";
    }))
  ];
  caches.cachix = [{
    name = "nix-community";
    sha256 = "1r0dsyhypwqgw3i5c2rd5njay8gqw9hijiahbc2jvf0h52viyd9i";
  }];
  home.packages = [ pkgs.binutils ]; # because gccemacs needs `as`

  # programs.emacs.package = pkgs.emacsGcc;
  programs.git.ignores = [
    "**/#*#"
    "*.elc"
    "*.rel"
    "*/ltximg/"
    "*_archive"
    "*_flymake.*"
    ".#*"
    ".cask/"
    ".dir-locals.el"
    ".org-id-locations"
    "/.emacs.desktop"
    "/.emacs.desktop.lock"
    "/auto/"
    "/elpa/"
    "/elpa/"
    "/eshell/*"
    "/network-security.data"
    "/server/"
    "auto-save-list"
    "dist/"
    "flycheck_*.el"
    "tramp"
  ];
  services.emacs.enable = true;

  programs.emacs = {
    enable = true;

    init = {
      enable = true;
      modules = {

        use-package = {
          precedence = 10;
          config = ''
            (eval-when-compile
              (require 'use-package)
            )
          '';
        };

        gcSettings = {
          precedence = 9;
          plugins = [ "gcmh" ];
          config = ''
            ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#avoid-garbage-collection-at-startup
            (setq gc-cons-threshold most-positive-fixnum
                  gc-cons-percentage 0.6
            )
            ;; Don't do in favor of gcmh
            ;; (defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
            ;; (add-hook 'emacs-startup-hook
            ;;   (lambda ()
            ;;     (setq gc-cons-threshold 16777216 ; 16mb
            ;;           gc-cons-percentage 0.1)))
            (use-package gcmh
              :config (gcmh-mode 1))
          '';
        };

        # https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
        # https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/
        earlyInit = {
          precedence = 8;
          plugins = [ ];
          config = ''
            ;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
            (push '(menu-bar-lines . 0) default-frame-alist)
            (push '(tool-bar-lines . 0) default-frame-alist)
            (push '(vertical-scroll-bars) default-frame-alist)
            (setq frame-inhibit-implied-resize t)
            (setq initial-major-mode 'fundamental-mode)
            (setq package-enable-at-startup nil ; don't auto-initialize!
            ;; this tells package.el not to add those pesky customized variable settings
            ;; at the end of your init.el
            package--init-file-ensured t)

            ;; Resizing the Emacs frame can be a terribly expensive part of changing the
            ;; font. By inhibiting this, we easily halve startup times with fonts that are
            ;; larger than the system default.
            (setq frame-inhibit-implied-resize t)

            ;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
            ;; compiled ahead-of-time when they are installed and site files are compiled
            ;; when gccemacs is installed.
            ;; (setq comp-deferred-compilation nil)
          '';
        };

        esup.config = ''
          (use-package esup
            :config
            (setq esup-depth 0)
          )
        '';

        flycheck.config = ''
          (use-package flycheck
            :hook (after-init . global-flycheck-mode)
          )'';
        which-key.config = ''
          (use-package which-key
            :config
            (which-key-mode))
        '';

        customInit = {
          plugins = [ ];
          config = builtins.readFile ./init.el;
        };

        customConfig = {
          plugins = [ ];
          config = ''
            (menu-bar-mode 0)
            (tool-bar-mode 0)
            (scroll-bar-mode 0)
            ;; (add-hook 'text-mode-hook 'display-line-numbers-mode)
            (add-hook 'prog-mode-hook 'display-line-numbers-mode)
            (add-hook 'prog-mode-hook 'hl-line-mode)
            (setq custom-file "~/custom.el")
            (load custom-file)
          '';
        };

        beacon.enable = false;
        beacon.config = ''
          (use-package beacon
            :hook (after-init . beacon-mode))
        '';
        # https://github.com/emacs-helm/helm/wiki
        helm.config = ''
          (use-package helm
            ;; TODO :defer t
            :config
            (require 'helm-config)
            (global-set-key (kbd "M-x") #'helm-M-x)
            (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
            (global-set-key (kbd "C-x C-f") #'helm-find-files)
            (helm-mode 1)
          )
        '';
        # TODO use use-package binds

        lsp-mode.config = ''
          (use-package lsp-mode
            :after general
            :hook (lsp-mode . lsp-enable-which-key-integration)
            :config
            (lmap "la" 'lsp-execute-code-action)
            (lmap "le" 'flycheck-list-errors)
            (lmap "lp" 'flycheck-previous-error)
            (lmap "ln" 'flycheck-next-error)
            (lmap "lr" 'lsp-workspace-restart)
            (setq read-process-output-max (* 1024 1024))
            :commands lsp)
        '';
        lsp-ui.config = ''
          (use-package lsp-ui
            :after lsp-mode general
            :config
            (lsp-ui-doc-mode nil)
            (setq lsp-ui-doc-delay 0)
            (lmap "ld" 'lsp-ui-doc-mode)
          )
        '';

        company.config = ''
          (use-package company
            :hook ('after-init . global-company-mode)
            :config
            (setq company-minimum-prefix-length 1)
            (setq company-idle-delay 0.2)
          )
        '';

        smartparens.config = ''
          (use-package smartparens
            :hook (after-init . smartparens-global-mode)
            :config
            (require 'smartparens-config)
            (show-smartparens-global-mode)
            (general-define-key
              :states 'insert
              "C-k" 'sp-up-sexp
              "C-l" 'sp-forward-sexp
              "C-M-l" 'sp-forward-slurp-sexp
              "C-M-h" 'sp-forward-barf-sexp
            )
          )
        '';

        # TODO yasnippet

        nix-mode.config = let doublesingle = "''"; in
          ''
            (use-package nix-mode
              :after reformatter
              :hook
              (nix-mode . nixpkgs-fmt-on-save-mode)
              :config
              (sp-with-modes 'nix-mode
                (sp-local-pair "=" ";")
                (sp-local-pair "'" nil :actions nil)
                (sp-local-pair "${doublesingle}" "${doublesingle}")
                (sp-local-pair "let" "in")
                (sp-local-pair "<" ">"))
            )
          '';

        markdown-mode.config = ''
          (use-package markdown-mode
            :mode "\\.md\\'"
          )
        '';

        magit.config = ''
          (use-package magit
            :general
            (:states 'normal :prefix "SPC g"
              "s" 'magit-status)
          )
        '';
        git-gutter.config = ''
          (use-package git-gutter
            :hook (after-init . global-git-gutter-mode)
            :after general
            :config
            (lmap "g a" 'git-gutter:stage-hunk)
            (lmap "g u" 'git-gutter:revert-hunk)
          )
        '';

        reformatter = {
          precedence = 1;
          config = ''
            (use-package reformatter
              :config
              (reformatter-define nixpkgs-fmt
                :program "nixpkgs-fmt"
                :lighter " NPF"))
          '';
        };

        # https://github.com/bbatsov/projectile
        projectile.config = ''
          (use-package projectile
            ;; TODO Defer
            :config
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            (setq projectile-ignored-project-function
              (lambda (file) (string-prefix-p "/nix/store/" file)))
          )
        '';
        helm-projectile.config = ''
          (use-package helm-projectile
            :after helm projectile general
            :config
            (lmap "f f" 'helm-projectile)
            (lmap "f g" 'helm-projectile-ag)
          )
        '';
        helm-ag.config = ''
          (use-package helm-ag
            :after helm-projectile
          )
        '';
      };
    };
  };
}
