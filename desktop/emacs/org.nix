{ pkgs, ... }:
{
  programs.emacs.init.modules = {
    org = {
      precedence = -1;
      packages = with pkgs; [
        texlive.combined.scheme-full
        etBook
      ];
      config = ''
        (setq org-hide-emphasis-markers t)
        (setq org-startup-indented t)
        (setq org-agenda-start-on-weekday nil)
        (setq sentence-end-double-space nil)

        (setq org-agenda-files '(
          "~/Org/agenda.org"
          "~/Org/birthdays.org"
        ))

        (setq system-time-locale "en-US")
        (setq org-hidden-keywords '(title))
        (setq org-fontify-quote-and-verse-blocks t)
        (setq org-todo-keywords '(
          (sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
        ))
        (setq-default prettify-symbols-alist '(
          ("#+BEGIN_SRC" . "λ") ("#+begin_src" . "λ")
          ("#+END_SRC"   . "ƛ") ("#+end_src"   . "ƛ")
          ("#+begin_quote"   . "\"") ("#+BEGIN_QUOTE"   . "\"")
          ("#+end_quote"   . "\"") ("#+END_QUOTE"   . "\"")
        ))
        (lmap
         "o c" 'org-capture
         "o a" 'org-agenda)

        (defvar jmc/org-capture-file "~/Org/agenda.org")

        (setq org-capture-templates `(
          ("a" "Agenda"       entry (file+datetree "~/Org/agenda.org") "* %?")
          ; ("j" "Journal"      entry (file+datetree "~/Org/journal.org") "* %(format-time-string \"%H:%M\")\n%?")
          ("d" "Diary"        entry (file+datetree "~/Org/diary.org") "* %(format-time-string \"%H:%M\")\n%?")
        ))
        (setq org-startup-indented t)

        (add-hook 'org-mode-hook (lambda ()
          (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
          (push '(?_ . ("_" . "_")) evil-surround-pairs-alist)
          (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)
          (visual-line-mode)
          (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))
          ;;(variable-pitch-mode)
          (prettify-symbols-mode)
          (setq line-spacing 0.2)
          (text-scale-set 1.0)
          (org-cycle-hide-drawers 'all)
        ))
      '';
    };

    org-roam.enable = false;
    org-roam.packages = with pkgs; [
      sqlite
      ripgrep
      graphviz
    ];
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
    org-fragtog.config = ''
      (use-package org-fragtog
        :hook (org-mode . org-fragtog-mode))'';
    org-superstar.config = ''
      (use-package org-superstar
        :hook (org-mode . org-superstar-mode)
      )
    '';
    writeroom = {
      plugins = [ "writeroom-mode" "visual-fill-column" ];
      config = ''
        (use-package writeroom-mode
          :hook
          (org-mode . writeroom-mode)
          (markdown-mode . writeroom-mode)
          :config
          (setq writeroom-maximize-window nil)
        )
      '';
    };
    org-sticky-header.config = ''
      (use-package org-sticky-header-mode
        :hook (org-mode . org-sticky-header-mode)
      )
    '';
    mixed-pitch.enable = false;
    mixed-pitch.config = ''
      (use-package mixed-pitch
        :hook
        (org-mode . mixed-pitch-mode)
      )
    '';
  };
} // import ../../lib/hm-git-sync-service.nix {
  inherit pkgs;
  name = "org-sync";
  dir = "~/Org";
}
