{ pkgs, ... }:
{
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
          (visual-line-mode)
        ))
      '';
    };

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
  systemd.user = {
    services.orgsync = {
      Unit.Description = "Org github sync";
      Service = {
        Type = "oneshot";
        ExecStart =
          let
            script = pkgs.writeShellScript "org-sync" ''
              cd ~/Org
              ${pkgs.gitAndTools.git-sync}/bin/git-sync
            '';
          in
          "${script}";
      };
    };
    timers.orgsync = {
      Unit.Description = "Org sync timer";
      Timer.OnCalendar = "*:0/15";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
