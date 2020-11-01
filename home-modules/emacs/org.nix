{ pkgs, ... }:
let
  tex = "${pkgs.texlive.combined.scheme-full}/bin/pdflatex";
  upkgs = import <unstable> { };
in
{
  home.packages = [
    pkgs.etBook
    pkgs.texlive.combined.scheme-full
  ];
  fonts.fontconfig.enable = true;
  programs.emacs.overrides = _: _: {
    org-fragtog = upkgs.emacsPackages.org-fragtog;
  };
  programs.emacs.init = {
    postlude = /* elisp */ ''
      (setq org-hide-emphasis-markers t)
      (setq org-startup-indented t)
      (push "~/Org/" org-agenda-files)
      (push "~/Org/CrossCompass/" org-agenda-files)
      ;; (setq system-time-locale "en-US")
      (global-prettify-symbols-mode t)
      (setq-default prettify-symbols-alist '(
        ("#+BEGIN_SRC" . "λ") ("#+begin_src" . "λ")
        ("#+END_SRC"   . "ƛ") ("#+end_src"   . "ƛ")
        ;; ("[ ]" . "☐") ("[X]" . "☑") ("[-]" . "❍")
      ))

      (defvar jmc/org-capture-file "~/Org/agenda.org")
      (defun jmc/get-agenda ()
        (interactive)
        (read-file-name-default
          "Capture to: "
          "~/Org/"
          nil
          'confirm
          "agenda.org"))
      
      (defun jmc/org-capture ()
        "Read file name to capture to."
        (interactive)
        (setq jmc/org-capture-file (jmc/get-agenda))
        (org-capture nil "j")
        (message "done"))
        ;; (call-interactively #'org-capture))

      (setq org-capture-templates `(
        ("j" "Journal"      entry (file+datetree jmc/org-capture-file) "* %?")
        ("J" "Journal+link" entry (file+datetree jmc/org-capture-file) "* %?\n  %i\n  %a")
        ("d" "Diary"        entry (file+datetree "~/Org/diary.org") "* %(format-time-string \"%H:%M\")\n%?")
      ))

      (setq org-agenda-start-on-weekday nil)

      (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

      (defun jmc/org-agenda ()
        "Make org-agenda for just the main agenda files"
        (interactive)
        (let ((org-agenda-files '("~/Org/agenda.org" "~/Org/birthdays.org")))
            (org-agenda nil "n"))
      )
    '';
    # (org-indent-mode)
    usePackage = {
      org.enable = true;
      org-bullets = {
        enable = true;
        after = [ "org" ];
        config = "(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))";
      };
      org-plus-contrib = {
        enable = false;
        after = [ "org" ];
      };
      org-contacts = {
        enable = false;
        after = [ "org-plus-contrib" ];
        config = ''(setq org-contacts-files '("~/Org/contacts.org))'';
      };
      org-fragtog = {
        enable = true;
        after = [ "org" ];
        config = "(add-hook 'org-mode-hook 'org-fragtog-mode)";
      };
    };
  };
}

# References:
# [1] https://github.com/kaushalmodi/.emacs.d/blob/42831e8997f7a3c90bf4bd37ae9f03c48277781d/setup-files/setup-org.el#L413-L584
