;;;; Scheme mode

(add-hook 'scheme-mode-hook
 (lambda ()
   (set (make-local-variable 'kill-whole-line) nil)
))

(defun chicken-doc ()
  (interactive)
  (let ((func (current-word)))
    (if func
	(process-send-string "*scheme*"
         (concat "(require-library chicken-doc) ,doc " func "\n")))))

(use-package quack :defer t)
(eval-after-load 'scheme
  '(progn
     (require 'quack)
     (define-key scheme-mode-map "\C-cd" 'chicken-doc)))

;;;; paredit

(use-package paredit
  :commands paredit-mode
  :hook
  ((scheme-mode emacs-lisp-mode)
   ;; Paredit signals an error on unbalanced parens, which is a bit antisocial because
   ;; it stops hook execution cold, leaving the buffer partially initialized.
   ;; Instead, tell the user what happened and continue.
   . (lambda ()
       (condition-case err
           (paredit-mode +1)
         (error (message "Paredit mode disabled: %s"
                         (error-message-string err))))))
  :config
  (define-key paredit-mode-map (kbd ")")   'paredit-close-parenthesis)  ;; Swap ) and M-)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-close-parenthesis-and-newline)
  ;; Terminal doesn't like C-) etc., so rebind frequently-used commands below.
  (define-key paredit-mode-map "\C-c)"     'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map "\C-c}"     'paredit-forward-barf-sexp)
  (define-key paredit-mode-map "\C-c("     'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map "\C-c{"     'paredit-backward-barf-sexp)
  (define-key paredit-mode-map "\M-R"      'paredit-splice-sexp-killing-backward)
  )

;;;; eggdoc

;; I don't think this can be activated buffer-local.
;; Additionally, M-x enable-eggdoc-indent is not found, must use M-: .
(defconst *eggdoc-indent-keywords* '(subsection section record procedure parameter))
(defun indent-scheme-keywords (keywords level)
  (mapc #'(lambda (name)
	    (put name 'scheme-indent-function level))
	keywords))
(defun enable-eggdoc-indent ()
  (indent-scheme-keywords *eggdoc-indent-keywords* 1))
(defun disable-eggdoc-indent ()
  (indent-scheme-keywords *eggdoc-indent-keywords* nil))
(defun copy-scheme-indent (from to)
  (put to 'scheme-indent-function 
       (get from 'scheme-indent-function)))
(defun copy-scheme-indents (indents)
  (mapc #'(lambda (x) (copy-scheme-indent (car x) (cadr x)))
	indents))

;;;;; Always indented keywords
;; This is here so it appears after indent-scheme-keywords is defined.
(indent-scheme-keywords '(and-let* let-location match-let) 1)
(indent-scheme-keywords '(handle-exceptions with-renamed foreign-lambda*) 2)
;; These are for ER-macros
(copy-scheme-indents 
 '((begin %begin) (let %let) (lambda %lambda) (case %case) (cond %cond)
   (foreign-lambda* %foreign-lambda*)
   ))
(copy-scheme-indents
 '((let let-prepare) (call-with-input-file call-with-database)
   (with-input-from-file with-transaction)
   ))
;; special case for ER defines; see scheme-indent-function in scheme.el
(put '%define 'scheme-indent-function 'defun)
;; dedent modules to column 0
(defun scheme-module-indent (state indent-point normal-indent) 0)
(eval-after-load 'scheme
 ;; May want to move other indent assigns to eval-after-load.
 '(put 'module 'scheme-indent-function 'scheme-module-indent))
(provide 'init-scheme)
