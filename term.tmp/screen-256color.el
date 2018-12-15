;; obtained on 2010-11-11 from
;; http://old.nabble.com/Add-%22lisp-term-screen-256color.el%22-for-GNU-Screen's-256-color-mode--td22117277.html

;; loads the xterm 256 color support even when TERM=screen-256colors
;; otherwise a basic terminal is loaded, which supports only 8 colors

(load "term/xterm")  ;; e.g. file:/Users/jim/local/homebrew/Cellar/emacs/23.2/share/emacs/23.2/lisp/term/xterm.el.gz

;; (defun terminal-init-screen () 
;;    "Terminal initialization function for screen." 
;;    ;; Use the xterm color initialization code. 
;;    (xterm-register-default-colors) 
;;    (tty-set-up-initial-frame-faces)) 

;; 2013-09-29 Actually call the full xterm init code instead.
(defun terminal-init-screen () 
   "Terminal initialization function for screen." 
   ;; Use the xterm initialization code.   This loads both 
   ;; 256 color support and extended xterm key support.
   (tty-run-terminal-initialization (selected-frame) "xterm")

   ;; iTerm sends these codes instead of standard xterm codes
   ;; for some reason.  Alternatively, configure iTerm to
   ;; send 1;4A - 1;4D for these codes.
   (define-key input-decode-map "\e[1;10A" [M-S-up])
   (define-key input-decode-map "\e[1;10B" [M-S-down])
   (define-key input-decode-map "\e[1;10C" [M-S-right])
   (define-key input-decode-map "\e[1;10D" [M-S-left])

   ;; iTerm must be manually configured to send 1;7A - 1;7D for C-M-arrow
   ;; and 1;8A - 1;8D for C-M-S-arrow.

   )
