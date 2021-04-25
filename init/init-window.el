
;; I prefer C-l, C-h etc. but these are overridden by certain
;; modes (e.g. cmuscheme, org).  One option is to use a special minor mode as in
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; although lazy-loaded major modes may need to have this re-set after load.

(use-package windmove
  :commands windmove-find-other-window
  :bind (("C-c l" . windmove-right)
         ("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up))
  :config
  (setq windmove-wrap-around t))

(use-package ace-window
  ;; M-o is bound to package facemenu (??)
  :bind (("M-o" . ace-window))
  :config
  (setq aw-scope 'frame)        ;; Less confusing when multiple emacsclient ttys are active.
  ;; Many extra ace-window commands (split, rebalance) can be
  ;; done as easily with standard commands, especially when there are only two windows (delete-other), or with the mouse (shrink, enlarge).
  ;; Binding other-window to dedicated keys, and windmove U/L/D/R bound to a hydra,
  ;; might be good enough.
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-dispatch-always nil)
  ;; Meta keys do not work in aw-dispatch-alist
  (add-to-list 'aw-dispatch-alist '(?o aw-flip-window)) ;; Pops aw ring! Ignores window motion outside aw.
  (add-to-list 'aw-dispatch-alist '(?t aw-split-window-fair "Split window fairly"))
  (add-to-list 'aw-dispatch-alist '(?= balance-windows))  ; C-x +
)

(defhydra hydra-windows (global-map "C-c w")
                                    ;; :timeout 5)
  "window"
  ("l" windmove-right)
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("n" split-window-below "new")
  ("v" split-window-horizontally "vert")
  ("x" delete-window "del")
  ;("0" delete-window "del")
  ("X" kill-this-buffer "kill")     ;; (d might be better) ;; should also close the window?
  ("b" bs-show "buffers" :color blue)  ;; exit immediately
  ("s" save-buffer "save")
  ("q" nil)   ; comment to allow q to pass through (e.g. to help window)
  ("L" hydra-move-splitter-right "split>")
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  )

;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))
(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))
(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))
(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(provide 'init-window)
