## Notes

    M-x describe-personal-keybindings : Show overrides done with bind-key and :bind
    M-x locate-library RET org RET : Get filesystem location of package "org"
    M-x list-load-path-shadows : List Emacs Lisp files that shadow other files
    `kill -USR2 emacs-pid`  if Emacs hangs; then use `M-x toggle-debug-on-quit` afterward.
    repeat last command:             C-x z   (z to repeat further),   M-x repeat
    repeat minibuffer command:       C-x M-:  or C-x ESC ESC  or M-x repeat-complex-command (like M-x M-p, but keeps arguments, as a lisp expression)
    show minibuffer command history: M-x command-history
    show last 100 chars typed:       M-x view-lossage, C-h l

### Lisp notes

    ;; insert test1 .. test5 into buffer
    (mapcar (lambda (x) (insert "test" (format "%d" x) "\n")) '(1 2 3 4 5))
    (let ((line 1)) (while (<= line 5) (insert "test" (format "%d" line) "\n") (setq line (+ line 1))))


