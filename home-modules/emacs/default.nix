# https://gitlab.com/rycee/nur-expressions/-/blob/master/hm-modules/emacs-init.nix
# https://github.com/jwiegley/use-package
# https://github.com/hlissner/doom-emacs/blob/develop/docs/modules.org

{ pkgs, ... }:
let
  nurNoPkgs = import <nur> { pkgs = null; };
  upkgs = import <unstable> { };
in
{
  imports = [
    nurNoPkgs.repos.rycee.hmModules.emacs-init
    ./haskell.nix
    ./org.nix
    ./ui.nix
  ];

  home.packages = with pkgs; [
    shellcheck
    ripgrep
    fd
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
    ".projectile"
    ".dir-locals.el"
    "/network-security.data"
  ];

  services.emacs = {
    enable = true;
    # FIXME client.enable = true;
  };

  programs.emacs = {
    enable = true;
    overrides = _: _: {
      neuron-mode = upkgs.emacsPackages.neuron-mode.overrideAttrs (
        old: {
          src = builtins.fetchGit {
            url = "https://github.com/felko/neuron-mode.git";
            rev = "43dfd9e21ee4aaf904f92965c93ad1075af1b4c7";
          };
        }
      );
      selectrum = upkgs.emacsPackages.selectrum;
      prescient = upkgs.emacsPackages.prescient;
      selectrum-prescient = upkgs.emacsPackages.selectrum-prescient;
      # ivy = upkgs.emacsPackages.ivy; # Doesn't display actions, fixed 2020-05-01
    };
    package = upkgs.emacs;

    init = {
      enable = true;
      recommendedGcSettings = true;

      prelude = ''
        (menu-bar-mode 0)
        (tool-bar-mode 0)
        (scroll-bar-mode 0)
        (global-display-line-numbers-mode t)
        (setq custom-file "~/custom.el")
        (load custom-file)
        (setq flymake-run-in-place nil)
      '';

      postlude = builtins.readFile ./init.el;

      usePackage = {

        no-littering = {
          enable = true;
          config = ''
            (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
          '';
        };

        polymode = {
          enable = false;
          after = [ "nix" ];
          config = /* elisp */ ''
            (add-to-list 'auto-mode-alist '("\\.nix$" . poly-nix-mode))
            
            (define-hostmode poly-nix-hostmode :mode 'nix-mode)
            
            (define-innermode poly-elisp-expr-nix-innermode
              :mode 'emacs-lisp-mode
              :head-matcher "/\\* elisp \\*/ '''\n"
              :tail-matcher " *''';\n"
              :head-mode 'host
              :tail-mode 'host)
            
            (define-polymode poly-nix-mode
              :hostmode 'poly-nix-hostmode
              :innermodes '(poly-elisp-expr-nix-innermode))
          '';
        };

        projectile = {
          enable = true;
          config = ''
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            (setq projectile-ignored-project-function
              (lambda (file) (string-prefix-p "/nix" file)))
          '';
        };

        avy.enable = true;

        nix-mode = {
          # Also check out
          # https://github.com/shlevy/nix-buffer
          # https://github.com/travisbhartwell/nix-emacs
          enable = true;
          # mode = [ "\\.nix\\'" ];
          # FIXME why does this not work when using :custom?
          init = ''
            (customize-set-variable 'nix-nixfmt-bin "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt")
          '';
        };

        ivy = {
          enable = false;
          config = "(ivy-mode 1)";
        };

        # Companion to ivy that at least fixes the M-x keys
        counsel = {
          enable = false;
          config = "(counsel-mode)";
        };

        selectrum = {
          enable = true;
          config = "(selectrum-mode +1)";
        };
        prescient.enable = true;
        selectrum-prescient = {
          enable = true;
          after = [ "selectrum" "prescient" ];
          config = "(selectrum-prescient-mode)";
        };

        counsel-projectile = {
          enable = false;
          config = ''
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
          '';
        };

        multiple-cursors.enable = true;
        magit.enable = true;
        key-chord = {
          enable = true;
          config = "(key-chord-mode t)";
        };

        # switch for https://github.com/raxod502/apheleia
        reformatter.enable = true;

        hydra.enable = true;
        highlight-symbol = {
          enable = true;
          config = "(setq highlight-symbol-idle-delay 0.3)";
        };
        pretty-hydra.enable = true;
        expand-region.enable = true;
        company.enable = true;

        smartparens = {
          enable = true;
          config = ''
            (smartparens-global-mode t)
            (show-smartparens-global-mode t)
          '';
        };

        evil = {
          enable = false;
          init = "(setq evil-want-C-u-scroll t)";
          config = "(evil-mode 1)";
        };
        evil-escape = {
          enable = false;
          config = ''
            (evil-escape-mode t)
            (setq-default evil-escape-key-sequence "hj")
          '';
        };

        popwin = {
          enable = false;
          config = ''
            (popwin-mode 1)
          '';
        };

        which-key = {
          enable = true;
          config = ''
            (which-key-mode)
            (which-key-setup-side-window-right-bottom)
          '';
        };

        neuron-mode = {
          enable = true;
          config = ''
            (setq-default neuron-default-zettelkasten-directory "/home/jmc/slipbox")
          '';
        };

      };
    };
  };
}
