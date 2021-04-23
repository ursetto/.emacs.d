;;;; ccmode

;; Except for c-offset-alist, the alists completely override any inherited
;; values from their parents.  E.g., c-hanging-braces-alist must be fully
;; specified.  For that reason, it's easier to just specify everything
;; completely rather than inheriting.

;; cc-mode uses c-beginning-of-defun when scanning internally for beginning of C
;; function.  This contrasts with the doc which claims beginning-of-defun [see
;; ccmode.info, 14 Performance Issues].  Therefore a { in column 0 should not
;; improve syntax parsing speed.  We do bind C-M-a to beginning-of-defun
;; to get the nicer defun-open search behavior.

;; font-lock-beginning-of-syntax-function, in ccmode, is set to c-beginning-of-syntax
;; as opposed to beginning-of-defun (19.3 Font Lock mode).  So left margin brace
;; should not improve scan speed either.

(defconst jim-c-style
  '(  ;; taken mostly from "linux" style with some KOS influence
    (c-basic-offset         . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-entry-open)
			       (brace-list-close)
			       (brace-entry-close)
			       (brace-list-intro)
			       (substatement-open after)
			       (block-close . c-snug-do-while)
			       (defun-open after) ; May slow down font lock if { is not in column 0.
			       (class-open after) 
			       (class-close)))
    (c-cleanup-list         . (brace-else-brace))
    (c-offsets-alist        . ((statement-block-intro . +)
			       (knr-argdecl-intro . 0)
			       (substatement-open . 0)
			       (substatement-label . 0)
			       (label . 0)
			       (statement-cont . +)
			       (case-label . *)
			       (statement-case-intro . *)
			       (arglist-cont-nonempty ;c-lineup-string-cont
						      c-lineup-argcont   ; subsumes string-cont
						      c-lineup-arglist)
			       (arglist-cont ;c-lineup-string-cont 
					     c-lineup-argcont
					     c-lineup-arglist)  )))
  "Jim's C style")

(add-hook 'c-mode-hook 
  (lambda () 
    (setq indent-tabs-mode nil)
    (define-key c-mode-base-map "\C-m" 'c-context-line-break) ;; newline-and-indent
    (define-key c-mode-base-map "\C-\M-a" 'c-beginning-of-defun)
    (define-key c-mode-base-map "\C-\M-e" 'c-end-of-defun)
    (c-toggle-auto-hungry-state 1)  ;; turn auto-hungry (C-c C-t) on
                                    ;; == auto-newline (C-c C-a) + hungry-delete (C-c C-d)
    (c-set-style "jim")             ;; set my style for the current buffer
                                    ;; Also possible to use (c-add-style "jim" jim-c-style t)
))

(add-hook 'c++-mode-hook       ;; Currently equivalent to c-mode-hook; could use c-mode-common-hook instead
  (lambda () 
    (setq indent-tabs-mode nil)
    (define-key c-mode-base-map "\C-m" 'c-context-line-break) ;; newline-and-indent
    (define-key c-mode-base-map "\C-\M-a" 'c-beginning-of-defun)
    (define-key c-mode-base-map "\C-\M-e" 'c-end-of-defun)
    (c-toggle-auto-hungry-state 1)  ;; turn auto-hungry (C-c C-t) on
                                    ;; == auto-newline (C-c C-a) + hungry-delete (C-c C-d)
    (c-set-style "jim")             ;; set my style for the current buffer
                                    ;; Also possible to use (c-add-style "jim" jim-c-style t)
))

(eval-after-load "cc-mode"
  '(c-add-style "jim" jim-c-style nil))

(provide 'init-c)
