{ pkgs, ... }:
let
  unstable = import <unstable> { };
in
{
  imports = [
    ./init.nix
    ./evil.nix
    ./haskell.nix
    ./org.nix
    ./ui.nix
  ];

  programs.git.ignores = [
    "**/#*#"
    "*.elc"
    "auto-save-list"
    "/.emacs.desktop"
    "/.emacs.desktop.lock"
    "tramp"
    ".#*"
    ".org-id-locations"
    "*_archive"
    "*_flymake.*"
    "/eshell/*"
    "/elpa/"
    "/elpa/"
    "*.rel"
    "/auto/"
    ".cask/"
    "dist/"
    "flycheck_*.el"
    "/server/"
    ".dir-locals.el"
    "/network-security.data"
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
          packages = [ "gcmh" ];
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
          packages = [ ];
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

        which-key.config = ''
          (use-package which-key
            :config
            (which-key-mode))
        '';

        customInit = {
          packages = [ ];
          config = builtins.readFile ./init.el;
        };

        customConfig = {
          packages = [ ];
          config = ''
            (menu-bar-mode 0)
            (tool-bar-mode 0)
            (scroll-bar-mode 0)
            (add-hook 'text-mode-hook 'display-line-numbers-mode)
            (add-hook 'prog-mode-hook 'display-line-numbers-mode)
            (setq custom-file "~/custom.el")
            (load custom-file)
          '';
        };

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
            :hook (lsp-mode . lsp-enable-which-key-integration)
            :commands lsp)
        '';
        lsp-ui.config = ''
          (use-package lsp-ui
            :after lsp-mode
          )
        '';

        company.config = ''
          (use-package company
            :hook ('after-init . global-company-mode))
        '';

        smartparens.config = ''
          (use-package smartparens
            :hook (after-init . smartparens-global-mode)
            :config
            (require 'smartparens-config)
            (general-define-key
              :states 'insert
              "C-l" 'sp-forward-sexp)
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
