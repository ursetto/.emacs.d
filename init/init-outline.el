;;;; outline-mode

;;;;; Comments
;; TAB (Overview) does not work for ELisp files; the whole file is collapsed into ...
;; That's because OVERVIEW does (hide-sublevels 1) [C-c @ C-q] and the ;;; lines
;; are at minimum level 3.  Modified outline-magic.el to take a minimum-level
;; variable.

;; To show more than 1 header in an overview, use C-u # M-x hide-sublevels.
;; 9 may be a reasonable number to show quite a bit [5 is minimum for default regexp].
;; Note that you can modify outline-cycle-minimum-header-level to do this automatically
;; for OVERVIEW.

;; I have now added a lisp-outline-level override [superseding lisp-mode.el] which
;; counts the semicolons and subtracts 2.  Therefore, ;;; counts as level 1.

;; Setting outline-minor-mode: 1 in local variables list fails.  The hook does not appear
;; to be called correctly -- either cycling doesn't work, or the ... suffix is not displayed.
;; I am forced to put it in the mode hook.

;;;;;; Outline styles
;; (setq outline-regexp ";;;+ [^ 	\n]\\|(")
;;    In this style ;;; are top-level headers and ;;;; ;;;;; etc. are subheaders.
;;    Toplevel ( are folded [in conjunction with scheme-outline-level below, level is
;;    set to 1000, guaranteeing toplevel forms will be expanded last.]
;;    No one uses this style except me, apparently.
;; (setq outline-regexp ";;; [^ 	\n]\\|(")
;;    ;;; are top-level headers; there are no subheaders.  This helps when people
;;    use ;;;; as the file header and ;;; as regular headers, which is common
;;    in the Chicken world.
;; Alternatively, we can still match ;;;+ and have scheme-outline-level return
;; level 1 for any number of semis, so ;;; ;;;; ;;;;; etc. are all considered
;; toplevel headers.  Not perfect, but better for Chicken use.
;; 
;; (setq outline-regexp ";;;;+ [^ 	\n]\\|(")
;;    Follows Riastradh's style guide wherein ;;;; is a toplevel header and ;;;;;+
;;    are subheaders, and ;;; are merely toplevel comments.
;;    May not work correctly with existing lisp-outline-level, which assigns level 1 to ;;;.
;;    Appears to be uncommon--people who use ;;; for toplevel comments (c.f. htmlprag)
;;    may use ;;; for headers, or skip headers entirely.
;; None of these address the no-header issue -- if the first line is not a header,
;; outline-mode collapses the entire document and you cannot cycle, only expand it
;; completely with (show-all).  The current solution is simply to not collapse
;; the document automatically on load, and rely on the user to not collapse
;; headerless documents manually.


;;;;; Customization
(add-hook 'emacs-lisp-mode-hook
          (function (lambda ()
                      (outline-minor-mode t)
		      ;; Default regexp includes min ;;; plus space plus first char, totalling 5 chars
 		      (setq outline-level 'lisp-outline-level)   ; the default
                      (when (fboundp 'outline-cycle)        ; only if outline-magic is loaded
                        (outline-cycle nil) ;; OVERVIEW mode, but won't respect local min-level yet
                        (setq outline-cycle-min-level 1))
					; (hide-other)
                      )))
(eval-after-load 'emacs-lisp-mode
  '(progn (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)))

;; This hook is only for outline-minor-mode related stuff!
(add-hook 'scheme-mode-hook
          (function (lambda ()
                      (outline-minor-mode t)
		      (setq outline-regexp ";;;+ [^ 	\n]\\|(")   ;; similar to emacs-lisp-mode
 		      (setq outline-cycle-min-level 1)
 		      (setq outline-level 'scheme-outline-level)
                      (idle-highlight-mode t)
		      ;; Enable scheme-complete on TAB and move outline cycling from
		      ;; TAB to M-TAB.  Warning, this is global, and should not be done.
;; 		      (define-key outline-minor-mode-map [(tab)] nil)
; 		      (outline-cycle nil) ;; OVERVIEW mode
; 		      (outline-cycle nil) ;; CONTENTS mode
                      )))

;; This is an unfortunate hack to restore the binding of little "tab", because
;; is hijacked everywhere by (require 'outline-magic).  org-mode only binds
;; big TAB (ctrl-i) and expects little "tab" (tab key) to reflect to big TAB.
;; Otherwise we can't create tables.  Note, orgtbl-mode still works without
;; this hack, because it intercepts (tab) directly.
(add-hook 'org-mode-hook
	(lambda ()
		(define-key org-mode-map [(tab)] 'org-cycle)
		;(setq org-allow-space-in-links nil)
        ))

;; - Enable outline cycling.
;; (add-hook 'outline-mode-hook 
;;   (lambda () (require 'outline-magic)))  ;; disabled due to conflicts with org mode cycling,
                                            ;; but org-mode can cycle OVERVIEW with S-Tab.


;; Set up outline-magic.  Comment out entire hook if not installed.  Note that missing outline-magic
;; may circularly prevent installing outline-magic from ELPA, probably due to outline use from elisp mode.
(use-package outline-magic :defer t)
(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
	    ;;(define-key outline-minor-mode-map [(tab)] 'outline-cycle)
	    (define-key outline-minor-mode-map (kbd "C-M-i") 'outline-cycle) ; <C-M-i> better than M-tab at terminal
	    (define-key outline-minor-mode-map "\C-c\t" 'outline-cycle) ; meh
	    (setq outline-cycle-emulate-tab nil)
	    (define-key outline-minor-mode-map (kbd "<backtab>") ; <backtab> better than S-tab at terminal
	      '(lambda () (interactive) (outline-cycle '(4))))))  ;; magic '(4) is for OVERVIEW cycle

; Warning: in outline-minor-mode, tab emulation calls indent-relative,
; and will not honor any mode-specific indentation.  So I originally bound cycling
; to S-tab (unused in minor-mode).  Unfortunately this extends to
; org-mode and TAB will override the nice org-mode behavior with the
; simpler outline-magic behavior.  I tried changing indent-relative to
; indent-for-tab-command, but it did not work.
; Nevertheless: indent-for-tab-command fixes my gripe with TAB in outline-minor-mode,
; so TAB is now the default cycle key.
;; (setq outline-cycle-emulate-tab t)
(defun lisp-outline-level ()
  "Lisp mode `outline-level' function (modified so ;;; is level 1)."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (if (looking-at "(\\|;;;###autoload")
	1000
      (if (looking-at ";;;+")
	  (let (buffer-invisibility-spec)  ; idea from http://www.cs.unc.edu/~gb/Software.html
	    (save-excursion
	      (skip-chars-forward ";")
	      (- (current-column) 2) ;; count semis and subtract 2
	      ))   
	len))))

(defun scheme-outline-level ()
  "Scheme mode `outline-level' function (modified so ;;; is level 1)."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (if (looking-at "(\\|;;;###autoload")
	1000
      (if (looking-at ";;;+")
	  (let (buffer-invisibility-spec)  ; idea from http://www.cs.unc.edu/~gb/Software.html
	    (save-excursion
	      (skip-chars-forward ";")
	      1          ; any number of semis is considered level 1
	      ))   
	len))))

(provide 'init-outline)
