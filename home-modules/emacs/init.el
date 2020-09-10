;; https://dfeich.github.io/www/org-mode/emacs/2018/05/10/context-hydra.html
(define-key evil-normal-state-map (kbd "SPC") 'jmc/space-hydra/body)

; (key-chord-define-global "hj" 'jmc/edit-hydra/body)

(pretty-hydra-define
  jmc/edit-hydra
  (:title "edit hydra" :quit-key "q" :idle 1)
  ("pending..."
   (("SPC" jmc/space-hydra/body "space hydra" :color blue)))
  )

(pretty-hydra-define
  jmc/space-hydra
  (:title "space hydra" :quit-key "q" :color blue :idle 1)
  ("modes"
   (("s" jmc/hydra-smartparens/body "smartparens")
    ("m" jmc/major-mode-hydra "major mode")
    ("z" jmc/hydra-zoom/body "zoom")
    ("p" jmc/hydra-projectile/body "projectile")
    ("o" jmc/hydra-org/body "org")
    )
  ))

(defun jmc-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(pretty-hydra-define
  jmc/hydra-smartparens
  (:title "smartparens" :quit-key "q" :idle 1.0)
  ("navigation"
   (("j" sp-next-sexp "next sexp")
    ("k" sp-backward-sexp "prev sexp")
    ("l" sp-down-sexp "down sexp")
    ("h" sp-backward-up-sexp "up sexp")
    ("b" sp-beginning-of-sexp "sexp begin")
    ("e" sp-end-of-sexp "sexp end"))
   "moving"
   (("H" sp-backward-slurp-sexp ") ->")
    ("L" sp-forward-slurp-sexp ") <-")
    ("K" sp-forward-barf-sexp "( ->")
    ("J" sp-backward-barf-sexp "( <-"))
   "gastronomy"
   (("t" sp-transpose-sexp "transpose"))
   "control"
   (("s" smartparens-strict-mode "strict mode" :toggle t)
    )))

(defhydra jmc/hydra-zoom ()
  "zoom"
  ("l" text-scale-increase "in")
  ("h" text-scale-decrease "out"))

(pretty-hydra-define
  jmc/hydra-elisp
  (:title "elisp" :idle 1.0)
  ("eval"
   (("b" eval-buffer "buffer" :color blue))))

(pretty-hydra-define
  jmc/hydra-org-mode
  (:title "org mode" :quit-key "q" :idle 1.0)
  ("navigation"
   (
    ("j" org-next-visible-heading "down")
    ("k" org-previous-visible-heading "up")
    ("z" org-cycle "cycle")
    ("Z" org-shifttab "global cycle")
    )
   "moving"
   (("L" org-demote-subtree "Demote")
    ("H" org-promote-subtree "Promote")
    ("J" org-metadown "Move down")
    ("K" org-metaup "Move up")
    ("y" org-refile "refile")
    ; ("o" org-meta-return "new item") ; TODO: first move to EOL
    )
   "links"
   (("i" org-insert-link "insert link")
    ("o" org-open-at-point "open" :color blue)
    ("n" org-next-link "next")
    ("p" org-previous-link "prev")
    )
   "org"
   (("t" org-shiftright "cycle TODO")
    ("T" org-todo "set TODO")
    ("c" org-ctrl-c-ctrl-c "C-c")
    ("a" org-agenda "agenda" :color blue)
    )
   "control"
   (("r" org-mode-restart "restart"))
   )
  )

(pretty-hydra-define
  jmc/hydra-org
  (:title "org global" :quit-key "q" :idle 1.0 :color blue)
  (""
   (("s" org-store-link "store link")
    ("a" org-agenda "agenda")
    ("c" jmc/org-capture "capture")
    ("d" (org-capture nil "d") "diary")
    )))

(pretty-hydra-define
  jmc/hydra-projectile
  (:title "projectile" :quit-key "q" :idle 1.0 :color blue)
  (""
   (("p" counsel-projectile-switch-project "switch project")
    ("f" counsel-projectile "find file")
    ("t" neotree-projectile-action "neotree")
    )
   )
  )

(reformatter-define
 ormolu-format
 :program "ormolu"
 )

(pretty-hydra-define
  jmc/hydra-haskell
  (:title "haskell" :idle 1.0)
  ("navigation"
   (("i" haskell-navigate-imports :color blue)
    )
   "formatting"
   (("O" ormolu-format-buffer "ormolu buffer" :color blue)
    ("o" ormolu-format-region "ormolu region" :color blue))
  )
  )

(defun jmc/major-mode-hydra ()
  "Launch a hydra depending on the current major mode"
  (interactive)
  (cl-case major-mode
    ('emacs-lisp-mode (jmc/hydra-elisp/body))
    ('org-mode (jmc/hydra-org-mode/body))
    ('haskell-mode (jmc/hydra-haskell/body))
    ))

;; Workaround https://github.com/abo-abo/hydra/issues/349
(setq hydra--work-around-dedicated nil)

;; (level1
;;  (level2
;;   a1
;;   a2
;;   (l3 a3 a4)
;;   a5 a7))
