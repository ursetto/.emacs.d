;;;; dired-mode

;; Prefer 'gls' for dired, otherwise fall back to ls.
(let ((gnu-ls (executable-find "gls")))
  (if gnu-ls
      (setq insert-directory-program gnu-ls)))

(put 'dired-find-alternate-file 'disabled nil)   ; use 'a' to reuse dir buffer
(define-key global-map "\C-x\C-j" 'dired-jump)   ; allow dired-jump before dired is loaded
(autoload 'dired-jump "dired" "Load dired when dired-jump is used." t)

;; Load Dired-x when Dired is loaded.
(add-hook 'dired-load-hook
	  (lambda () 
	    (require 'dired-x)
	    ;; set dired-x global vars here
	    (define-key dired-mode-map "o" 'dired-open-mac)  ; use 'o' to /usr/bin/open files
	    ))
(setq default-dired-omit-mode 0)   ; 1 for omit by default, 0 for non-omit mode
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; set dired-x buffer-local vars here
 	    (dired-omit-mode default-dired-omit-mode)
	    ;; use M-o to toggle omit
	    ))

;; Enter virtual dired mode on files ending with .dired.  Useful for
;; saving a dired buffer for later or for importing an ls -lR or
;; find listing from the command line.
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
			    auto-mode-alist))

;; Jump to directory of current buffer and kill the existing buffer--
;; similar to C-x C-v C-k RET.  Author: yours truly.
;; Evidently unused.
(defun dired-kill-and-jump ()
  (interactive)
  (let ((oldbuf (current-buffer)))
    (dired-jump)
    (kill-buffer oldbuf)))

;; Open file using /usr/bin/open.
(defun dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
	(call-process "/usr/bin/open" nil 0 nil file-name))))

;;;;; Quickly browse through files in dired (http://stackoverflow.com/a/19933337)
;; Fixes: dired-next-line, dired-previous-line require an argument
;; Changes: use 'v' to view (not ^o, which we expect to edit in another window)

(add-hook 'dired-mode-hook
  (lambda()
    (define-key dired-mode-map (kbd "v") 'dired-view-current)     ; was dired-display-file
    (define-key dired-mode-map (kbd "n")   'dired-view-next)           ; was dired-next-line
    (define-key dired-mode-map (kbd "p")   'dired-view-previous))) ; was dired-previous-line

(defun dired-view-next ()
  "Move down one line and view the current file in another window."
  (interactive)
  (dired-next-line 1)
  (dired-view-current))

(defun dired-view-previous ()
  "Move up one line and view the current file in another window."
  (interactive)
  (dired-previous-line 1)
  (dired-view-current))

(defun dired-view-current ()
  "View the current file in another window (possibly newly created)."
  (interactive)
  (if (not (window-parent))
      (split-window-horizontally
       ))                                   ; create a new window if necessary
  (let ((file (dired-get-file-for-visit))
        (dbuffer (current-buffer)))
    (other-window 1)                                          ; switch to the other window
    (unless (equal dbuffer (current-buffer))                 ; don't kill the dired buffer
      (if (or view-mode (equal major-mode 'dired-mode))   ; only if in view- or dired-mode
          (kill-buffer)))                                                    ; ... kill it
    (let ((filebuffer (get-file-buffer file)))
      (if filebuffer                              ; does a buffer already look at the file
          (switch-to-buffer filebuffer)                                    ; simply switch 
        (view-file file))                                                    ; ... view it
      (other-window -1))))                   ; give the attention back to the dired buffer

;; https://stackoverflow.com/a/18885461/4289268
;; Note 'dired-create-empty-file' will be available in emacs 27.1.
(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "_") 'my-dired-create-empty-file)
     (defun my-dired-create-empty-file (file)
       "Create a file called FILE.
If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))

(provide 'init-dired)
