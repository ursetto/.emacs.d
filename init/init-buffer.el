;;;; bs (buffer mode)

(use-package bs
  :config
  ;; Here we set the bs-must-show-regexp value of the "files" configuration to
  ;; always show the *scheme* buffer and any *info* buffers.
  (setcar (cdr (assoc "files" bs-configurations))
	  (concat "^\\*scheme\\*$"  "\\|"  ;; concat: elisp sec 4.3
		  "^\\*info\\*"))
  ;; Note: bs-sort-functions and bs-attributes-list have to be customized
  ;; (like bs-configurations). But customizing these is best done through
  ;; the menu system.
  )

(bind-key "C-x C-b" 'bs-show)

;; Stable sort when sorting by filenames.  Originally from
;; http://os.inf.tu-dresden.de/~mp26/download/swbuff-y.el
;; and cleaned up by me.  Customize bs-sort-functions to activate.
;; Note: Cloned file buffers don't have a filename and will not be grouped "correctly".
(defun bs--sort-by-filename-stable (b1 b2)
  "Compare buffers B1 and B2 by file name and as a secondary condition
   by buffer name."
  (if (string-equal (buffer-file-name b1) (buffer-file-name b2))
      (string< (buffer-name b1)
               (buffer-name b2))
    (string< (or (buffer-file-name b1) "")
             (or (buffer-file-name b2) ""))))

(defun bs--sort-by-filename-stable-empty-end (b1 b2)
  "Compare buffers B1 and B2 by file name and as a secondary condition
   by buffer name.  Empty filenames placed at end."
  (or (string< (buffer-file-name b1) (buffer-file-name b2))
      (and (not (string< (buffer-file-name b2) (buffer-file-name b1)))
           (string< (or (buffer-name b1) "")
                    (or (buffer-name b2) "")))))

;; Stable sort for sort-by-mode -- my authorship.
(defun bs--sort-by-mode-stable (b1 b2)
  "Compare buffers B1 and B2 by mode name.  Secondarily, 
   sort by filename."
  (save-excursion
    (let ((m1 (progn (set-buffer b1) (format "%s" mode-name)))
	  (m2 (progn (set-buffer b2) (format "%s" mode-name))))
      (if (string-equal m1 m2)
	  (bs--sort-by-filename-stable b1 b2)
          (string< m1 m2)))))

;; Like bs--get-file-name but abbreviates homedir to ~.
;; Customize Files in bs-attributes-list to activate.
(defun bs--get-file-name-abbrev (start-buffer all-buffers)
  "Return string for column 'File' in Buffer Selection Menu.
This is the variable `buffer-file-name' of current buffer.
If current mode is `dired-mode' or `shell-mode' it returns the
default directory.  Filenames are abbreviated (e.g. homedir -> ~).
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (propertize (if (member major-mode '(shell-mode dired-mode))
                  default-directory
                (if buffer-file-name (abbreviate-file-name buffer-file-name) ""))
              'mouse-face 'highlight
              'help-echo "mouse-2: select this buffer, mouse-3: select in other frame"))

(provide 'init-buffer)
