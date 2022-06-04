;; Temporarily changing gc size and nulling handler list during startup
;; saves ~100ms in startup costs.
(defvar gc-cons-threshold--orig gc-cons-threshold)
(defvar file-name-handler-alist--orig file-name-handler-alist)
(setq gc-cons-threshold 100000000
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook      ; Reset these after we are done
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold--orig
                  file-name-handler-alist file-name-handler-alist--orig)
            ;; On a 24-bit screen, emacs will use the 24-bit color sequence to
            ;; display named colors 0-15, so the terminal can't intercept and
            ;; improve these colors. So, redefine emacs' conception of them.
            ;; These colors are over-precise (pulled from iTerm) and could probably
            ;; be rounded to their nearest 256-color equivalent, so we could potentially
            ;; use this on 256-color terminals without palette remapping.
            ;; You may also map to an existing color with (cdr (assoc "salmon" color-name-rgb-alist)).
            (if (> (display-color-cells) 256)
                (progn    ; can use cl-flet to abbrev functions https://emacs.stackexchange.com/a/33899/28382
                  (tty-color-define "red"            1 (zb/rgb-to-triple "#e85149"))
                  (tty-color-define "yellow"         3 (zb/rgb-to-triple "#e0c200"))
                  (tty-color-define "blue"           4 (zb/rgb-to-triple "#0058e8"))
                  (tty-color-define "magenta"        5 (zb/rgb-to-triple "#c762d4"))
                  (tty-color-define "brightred"      9 (zb/rgb-to-triple "#ff7472"))
                  (tty-color-define "brightgreen"   10 (zb/rgb-to-triple "#5ff967"))
                  (tty-color-define "brightyellow"  11 (zb/rgb-to-triple "#fefb43"))
                  (tty-color-define "brightblue"    12 (zb/rgb-to-triple "#5c97ff"))
                  (tty-color-define "brightmagenta" 13 (zb/rgb-to-triple "#ff9bff"))
                  (tty-color-define "brightcyan"    14 (zb/rgb-to-triple "#5ffdff"))
                  )
              )
            ))

;; Convert #XXYYZZ or #XYZ to (R G B), the triple expected by tty-color-define.
;; Swiped from tty-color-translate.
(defun zb/rgb-to-triple (color)
  (and (stringp color)
       (>= (length color))
       (eq (aref color 0) ?#)
       (member (aref color 1)
               '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
		    ?a ?b ?c ?d ?e ?f))
       (let* ((ndig (/ (- (length color) 1) 3))
			      (i1 1)
			      (i2 (+ i1 ndig))
			      (i3 (+ i2 ndig)))
         (list
          (lsh (string-to-number (substring color i1 i2) 16)
	       (+ 8 (* 4 (- 2 ndig))))
	  (lsh (string-to-number (substring color i2 i3) 16)
	       (+ 8 (* 4 (- 2 ndig))))
	  (lsh (string-to-number (substring color i3) 16)
	       (+ 8 (* 4 (- 2 ndig))))))))

;; early-init's main purpose is to set up the package manager.  In Emacs >= 27, that's
;; done implicitly at the end of early-init, but in earlier versions it happens at the end
;; of init. We'll start it ourselves later.

(setq package-enable-at-startup nil) ;; Prevent package-initialize from being called implicitly.
