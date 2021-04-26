;;;; AUCTex

;; With Skim installed, you can jump in Skim to the currently selected
;; line in Aquamacs by using Command-Shift-click or C-c C-c J RET.
;; You must first Customize Group AuCTeX:Tex Commands and change 'latex'
;; to 'latex -synctex=1'.
;; Also, you can reverse search (Skim->Aquamacs) with Command-Shift-Click
;; after you set Skim Preferences->Sync to know about Aquamacs [make sure
;; the path to emacsclient is correct; it may not be in your PATH].

(add-hook 'TeX-mode-hook
          (lambda ()
	    (outline-minor-mode t)
	    (define-key TeX-mode-map "\C-ct" 'TeX-command-master-LaTeX)
	    (define-key TeX-mode-map "\C-ce" 'LaTeX-emph-word)
	    (add-to-list 'TeX-output-view-style
			 ;; Use Skim.app for pdf output; though Jump to PDF uses Skim,
			 ;; regular View does not
			 '("^pdf$" "." "open -a Skim %o"))
	    ))
(setq TeX-parse-self t)

;; Run LaTeX without confirming it;
(defun TeX-command-master-LaTeX ()
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file nil)
  ;  (TeX-view)
)

(defun LaTeX-command-word (word)
  (interactive)
  (save-excursion
    (forward-word)
    (insert "}")
    (backward-word)
    (insert (concat "\\" word "{"))))

(defun LaTeX-command-region (word)
  (interactive)
  (let ((region (and transient-mark-mode mark-active)))
    (if region
        (save-excursion
	  (goto-char (region-end))
	  (insert "}")
	  (goto-char (region-beginning))
	  (insert (concat "\\" word "{")))
      (LaTeX-command-word word))))

(defun LaTeX-emph-word () 
  (interactive)
  (LaTeX-command-word "emph"))

(provide 'init-tex)
