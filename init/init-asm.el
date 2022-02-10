(use-package asm-mode
  :bind
  ;;;  Alternatively, bind forward-to-word to either M-f (confusing) or
  ;;;  perhaps TAB, using M-i as tab-to-tab-stop. (Works as simple solution,
  ;;;  but moving to next tab stop is nicer in practice.)
  (:map asm-mode-map
        ([?\t] . forward-to-word)
        ("C-c o" . overwrite-mode))
  :hook
  ((asm-mode . (lambda ()
                 (modify-syntax-entry ?# "w" asm-mode-syntax-table)
                 (modify-syntax-entry ?< "w" asm-mode-syntax-table)
                 (modify-syntax-entry ?> "w" asm-mode-syntax-table)
                 (modify-syntax-entry ?! "w" asm-mode-syntax-table)
                 (modify-syntax-entry ?@ "w" asm-mode-syntax-table) ; not critical
                 (setq-local tab-stop-list '(12 18 40))
                 (electric-indent-local-mode -1)
                 (setq tab-always-indent t)
                 (setq indent-line-function #'tab-to-tab-stop)
                 (setq comment-column 40)  ; sometimes this randomly doesn't work
                 )))
  )

;; Issues:
;;   Expansion of identifier after "#>" is not working. Disabling #<> as part of words in syntax table doesn't fix it.

;;; This fixes Ctrl-O, but breaks newline (RET) -- must use C-j.
;; (setq left-margin (car tab-stop-list))


;; Check asm-indent-line.
;; We should have ability to move cursor forward/backward one tab stop,
;; and if we reached end of line, insert a tab. This may behave unexpectedly
;; though as it will insert invisible tabs when we might expect just to
;; be moving fields. Thus "virtual" cursor motion might be needed.

;; Random things don't respond to tab-stop-list, such as using Ctrl-O
;; to insert a line. This uses fill-prefix or left margin.
;; (setq fill-prefix nil)  to disable, but will not work well.
;; Unclear how (newline) works as left-margin is disabled.

;; Overwrite-mode is useful but we need a keybinding and different cursor shape
;; or some other indicator.


(provide 'init-asm)
