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
                (progn
                  (tty-color-define "red"            1 '(59392 20736 18688))    ;; #e85149
                  (tty-color-define "blue"           4 '(    0 22528 59392))    ;; #0058e8
                  (tty-color-define "magenta"        5 '(50944 25088 54272))    ;; #c762d4
                  (tty-color-define "brightred"      9 '(65535 29696 29184))    ;; #ff7472
                  (tty-color-define "brightgreen"   10 '(24320 63744 26368))    ;; #5ff967
                  (tty-color-define "brightyellow"  11 '(65024 64256 17152))    ;; #fefb43
                  (tty-color-define "brightblue"    12 '(23552 38656 65535))    ;; #5c97ff
                  (tty-color-define "brightmagenta" 13 '(65535 39680 65535))    ;; #ff9bff
                  (tty-color-define "brightcyan"    14 '(24320 64768 65280))    ;; #5ffdff
                  )
              )
            ))

;; early-init's main purpose is to set up the package manager.  In Emacs >= 27, that's
;; done implicitly at the end of early-init, but in earlier versions it happens at the end
;; of init. We'll start it ourselves later.

(setq package-enable-at-startup nil) ;; Prevent package-initialize from being called implicitly.
