;;; Aquamacs

(when (featurep 'aquamacs)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq x-select-enable-clipboard t)

;;(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))  ;; open *help* in current frame

  ;; Alternative to (setq Man-notify-method 'newframe).  'newframe
  ;; will open a new frame every single time, even if a frame exists
  ;; displaying that man page (this is arguably a bug).  Instead we
  ;; set to 'aggressive mode to open it in a new -window-, then tell
  ;; Aquamacs to open all '*Man ' buffers in their own window.
  ;; Now M-x man and iswitchb (C-x b) operate correctly but
  ;; C-x C-b (bs-mode) does not--maybe a config option.
  (add-to-list 'special-display-regexps '("[ ]?\\*Man .*" (width . 78) (height . 40)))
;  (add-to-list 'special-display-regexps '("[ ]?\\*Man .*" man-display-buffer-in-man-frame))
    ; marks as dedicated -- I think this conflicts with our goals


  (setq Man-notify-method 'aggressive)

  ;; Transparency
  (setq transparency-level 94)
  (set-frame-parameter nil 'alpha transparency-level)  ; set alpha on current frame
  (add-hook 'after-make-frame-functions                ; set alpha on future frames
	    (lambda (selected-frame)
	      (set-frame-parameter selected-frame 'alpha transparency-level)))


  )

(provide 'init-aquamacs)
