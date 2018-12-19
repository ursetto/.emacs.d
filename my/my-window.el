
;; I prefer C-l, C-h etc. but these are overridden by certain
;; modes (e.g. cmuscheme, org).  One option is to use a special minor mode as in
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; although lazy-loaded major modes may need to have this re-set after load.

(use-package windmove
  :bind (("C-c l" . windmove-right)
         ("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up))
  :config
  (setq windmove-wrap-around t))

(use-package hydra)  ;; FIXME: raise to init.el

(defhydra hydra-windows (global-map "C-c w"
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
  ("K" kill-this-buffer "kill")     ;; should this also close the window?
  ("b" bs-show "buffers" :color blue)  ;; exit immediately
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
