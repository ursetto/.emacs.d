;;; cperl-mode

;; good values for cperl-array: sandybrown, tan, pink

;; Note: autoinsert is schizophrenic. { only is electric after
;; a () expression, so else { does not insert a newline.  You
;; must have cperl-electric-keywords enabled for this, which
;; will insert the whole construct.  sub { is never expanded.
;; Doubling braces/parens is silly, because you can't "type over"
;; the closing delimiter, but rather have to skip it with cursor
;; motion or use cperl-linefeed (C-c C-j).  The closing } in
;; an autoinserted construct must be skipped over manually.
;; After }, newline is always inserted and following 'else' is not
;; cleaned up, so you have to do this manually.  After ;, inserted
;; newline will not be deleted if you immediately type }, so you
;; have to delete it manually.
;; Tried 5.22 (newest), but compilation error, and when fixed, it
;; broke HEREDOC highlighting and had other weird font problems.
;; And it didn't fix any of these issues.
(add-hook 'cperl-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq cperl-indent-level 4)
    (setq cperl-highlight-variables-indiscriminately t)
    (setq cperl-electric-lbrace-space nil)
    (setq cperl-electric-parens nil)
    (setq cperl-auto-newline nil)
    (setq cperl-electric-keywords nil)
    (define-key cperl-mode-map "\C-m" 'newline-and-indent)
    (define-key cperl-mode-map "\C-j" 'cperl-linefeed)
))

(provide 'init-perl)
