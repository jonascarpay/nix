{ pkgs, ... }:
{
  # home.packages = [
  #   pkgs.etBook
  #   pkgs.texlive.combined.scheme-full
  # ];
  # fonts.fontconfig.enable = true;
  # programs.emacs.overrides = _: _: {
  #   org-fragtog = upkgs.emacsPackages.org-fragtog;
  # };
  home.packages = with pkgs; [
    sqlite
    ripgrep
    graphviz
  ];
  programs.emacs.init.modules = {
    org = {
      packages = [ ];
      precedence = -1;
      config = ''
        (setq org-hide-emphasis-markers t)
        (setq org-startup-indented t)
        (setq org-agenda-start-on-weekday nil)

        (setq org-agenda-files '(
          "~/Org/agenda.org"
          "~/Org/contacts.org"))

        (setq system-time-locale "en-US")
        (global-prettify-symbols-mode t)
        (setq-default prettify-symbols-alist '(
          ("#+BEGIN_SRC" . "λ") ("#+begin_src" . "λ")
          ("#+END_SRC"   . "ƛ") ("#+end_src"   . "ƛ")
        ))
        (lmap
         "o c" 'org-capture
         "o a" 'org-agenda)

        (defvar jmc/org-capture-file "~/Org/agenda.org")

        (setq org-capture-templates `(
          ("a" "Agenda"       entry (file+datetree "~/Org/agenda.org") "* %?")
          ("j" "Journal"      entry (file+datetree "~/Org/journal.org") "* %(format-time-string \"%H:%M\")\n%?")
          ("d" "Diary"        entry (file+datetree "~/Org/diary.org") "* %(format-time-string \"%H:%M\")\n%?")
          ;; ("j" "Journal"      entry (file+datetree jmc/org-capture-file) "* %?")
          ;; ("J" "Journal+link" entry (file+datetree jmc/org-capture-file) "* %?\n  %i\n  %a")
        ))
        (setq org-startup-indented t)

        (add-hook 'org-mode-hook (lambda ()
          (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
          (push '(?_ . ("_" . "_")) evil-surround-pairs-alist)
          (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)
        ))
      '';
    };
    # usePackage = {
    #   org.enable = true;
    #   org-bullets = {
    #     enable = true;
    #     after = [ "org" ];
    #     config = "(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))";
    #   };
    # org-fragtog.config = ''
    #   (use-package org-fragtog
    #     :after org
    #     :hook org-mode)
    # '';

    org-roam.config = ''
      (use-package org-roam
        :ensure t
        :after general
        :hook
          (after-init . org-roam-mode)
        :custom
          (org-roam-directory "~/Org/Notes/")
        :config
          (lmap
            "o r" 'org-roam
            "o f" 'org-roam-find-file
            "o g" 'org-roam-graph)
          (general-define-key
            :keymaps 'org-mode-map
            :states 'insert
            "C-l" 'org-roam-insert
            "C-L" 'org-roam-insert-immediate)
      )
    '';
  };
}

# References:
# [1] https://github.com/kaushalmodi/.emacs.d/blob/42831e8997f7a3c90bf4bd37ae9f03c48277781d/setup-files/setup-org.el#L413-L584
