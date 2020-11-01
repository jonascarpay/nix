;; https://dfeich.github.io/www/org-mode/emacs/2018/05/10/context-hydra.html
;; (define-key evil-normal-state-map (kbd "SPC") 'jmc/space-hydra/body)

(key-chord-define-global "hj" 'jmc/edit-hydra/body)

(pretty-hydra-define
  jmc/edit-hydra
  (:title "edit hydra" :quit-key "q" :idle 1 :color blue)
  ("pending..."
   (("SPC" jmc/space-hydra/body "space hydra" :color blue)
    ("f" hydra-avy/body "avy")
    ("c" hydra-multiple-cursors/body "mc")
    ("p" hydra-smartparens/body "sp" :color blue)
    )
   "Movement"
    (("g" beginning-of-buffer "jump top" :color red)
    ("G" end-of-buffer "jump bottom" :color red)
    )
   ))

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
  unused/jmc/hydra-smartparens
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
    ("A" org-agenda "agenda")
    ("a" jmc/org-agenda "default agenda")
    ("c" jmc/org-capture "capture")
    ("d" (org-capture nil "d") "diary")
    )))

(pretty-hydra-define
  jmc/hydra-projectile
  (:title "projectile" :quit-key "q" :idle 1.0 :color blue)
  (""
   ( ;; ("p" counsel-projectile-switch-project "switch project")
     ;; ("f" counsel-projectile "find file")
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

;; https://github.com/abo-abo/hydra/wiki/avy
(defhydra hydra-avy (:exit t :hint nil)
  "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
  ("c" avy-goto-char-timer)
  ("C" avy-goto-char)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

;; https://github.com/abo-abo/hydra/wiki/multiple-cursors
(defhydra hydra-multiple-cursors (:hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("|" mc/vertical-align)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

(defhydra hydra-smartparens (:hint nil)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)
  
  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)
  
  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)
  
  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)
  
  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))
