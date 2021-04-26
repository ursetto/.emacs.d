;;;; ocaml

;; Requires MELPA packages tuareg, utop and merlin; also `opam install
;; merlin`. We use company-mode now as auto-complete is unmaintained,
;; though the latter still works fine.
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'merlin-mode-hook 'company-mode)

;; utop phrase eval doesn't seem to work (consistently evaluates much
;; smaller regions than expected.)  May be that tuareg-discover-phrase
;; is broken.
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;; Use the opam installed utop. Doesn't seem necessary.
(setq utop-command "opam config exec -- utop -emacs")
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; Note: If you use 'easy setup mode, which turns off automatic completion
;; for some reason, you will probably want to set a trigger key via
;;    (ac-set-trigger-key "TAB"), or bind auto-complete to M-tab or C-c <tab>.
;; Alternatively just use 't mode to use your standard auto-complete config.
;; Warning: Must manually autoload auto-complete-mode, or (setq auto-complete-mode t)
;; in merlin-mode-hook, as merlin-ac will only be loaded after auto-complete is.
;(setq merlin-ac-setup t) 

(eval-after-load 'merlin
  '(progn
     ;; Can't get this to work regardless of what key it's on ... it just seems to
     ;; cancel the type enclosing query. It worked once, but never again.
     (define-key merlin-mode-map (kbd "M-<up>") 'merlin-type-enclosing-go-up)
     (define-key merlin-mode-map (kbd "M-<down>") 'merlin-type-enclosing-go-down)
     ;; merlin-document is useful and not bound by default. Also see C-c C-l.
     (define-key merlin-mode-map (kbd "C-c d") 'merlin-document)  ; or C-c m d
     ;; merlin-jump may also be worth binding. Try C-c m j, or C-c C-j.
     ))
             
;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)

(provide 'init-ocaml)
