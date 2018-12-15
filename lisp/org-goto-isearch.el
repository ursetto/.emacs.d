;; From http://article.gmane.org/gmane.emacs.orgmode/480 
;; Activates automatic isearch on C-c C-j (org-goto)

(defvar local-auto-isearch-map (make-sparse-keymap))
(set-keymap-parent local-auto-isearch-map isearch-mode-map)
(define-key local-auto-isearch-map "\C-i" 'isearch-other-control-char)

(defun local-search-forward-headings (string bound noerror)
  (catch 'return
    (while (search-forward string bound noerror)
      (when (save-match-data (outline-on-heading-p t))
	(throw 'return t)))))

(defun local-auto-isearch ()
  (interactive)
  (let ((keys (this-command-keys)))
    (when (eq (lookup-key isearch-mode-map keys) 'isearch-printing-char)
      (isearch-mode t)
      (isearch-process-search-char (string-to-char keys)))))

(defun local-move-tree (&optional tobuffer)
  (interactive)
  (setq tobuffer (or tobuffer (current-buffer)))
  (let ((toplace (save-excursion
		    (set-buffer tobuffer)
		    (beginning-of-buffer)
		    (org-goto)
		    (copy-marker (point)))))
    (org-cut-subtree)
      (save-excursion
	(set-buffer tobuffer)
	(goto-char toplace)
	(org-back-to-heading t)
	(org-paste-subtree (outline-level)))))

(add-hook 'org-mode-hook
	  '(lambda ()
	     (defadvice org-get-location (around auto-isearch-advice activate)
	       (let ((isearch-mode-map local-auto-isearch-map)
		     (isearch-hide-immediately nil)
		     (isearch-search-fun-function
		      (lambda () 'local-search-forward-headings)))
		 ad-do-it))
  	     (define-key org-goto-map [(return)] 'org-goto-ret)
	     (define-key-after org-goto-map [t] 'local-auto-isearch)
	     (require 'cl)
	     (dolist (key '(?n ?p ?f ?b ?u ?q))
	       (setq org-goto-map (assq-delete-all key org-goto-map)))))