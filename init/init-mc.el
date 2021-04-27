;;; multiple-cursors

;; https://github.com/magnars/expand-region.el
;; Expand selection to nearest word, quoted string or sexp. Press 'e' when region active.
(use-package expand-region :defer t)

;; The selected keymap is used while the region is active. Instead of binding global chords to
;; region commands, you bind single keystrokes in this map. This is mainly useful for complex
;; or repeated commands like expand-region or multiple-cursors; an active region acts like a hydra.
(use-package selected
  :init (selected-global-mode)
  :bind (:map selected-keymap
              ;; Take care these keys do not collide with multiple-cursors.
              ("e" . er/expand-region)
              ("RET" . (lambda () (interactive) (deactivate-mark)))  ; C-g is similar in mc mode
              ("q" . selected-off)    ; Disable selected bindings but leave region active. May not be useful.
              ("u" . upcase-region)   ; already on C-x C-u
              ("d" . downcase-region) ; already on C-x C-l
              (";" . comment-or-uncomment-region)  ; doesn't work if paredit loaded afterward
              ("w" . copy-region-as-kill)  ; Already on M-w. w might be better than c.
              (">" . indent-region)   ; on C-M-\
              ("k" . kill-region)     ; already on C-w. k might be better than w.
              ))

;; https://github.com/magnars/multiple-cursors.el
;; Almost all multiple-cursors commands operate on regions, so we bind them in selected-keymap.
;; The utility commands that do not work on regions (like mc/insert-numbers) or also work
;; outside regions (mc/reverse-regions) might be helpful to bind to a hydra on 'C-c m'.
;;
;; Notes: you will normally mark a consecutive region of lines and then use (l/^/$) to place a cursor
;; on each one, or you will mark text to search for and use (m/n/p/N/P/./,) to place cursors at matches.
;; Press RET or C-g to deselect the region and go back to typing; press C-g again to exit mc mode.
;; It's not possible to place or remove arbitrary cursors; creative text search, cursor
;; motion and unmark/skip may be needed.
(use-package multiple-cursors
  :bind (; ("C-c m l" . mc/edit-lines)                 ; extraneous; see selected-keymap
         ; ("C-c m n" . mc/mark-next-like-this-word)   ; extraneous; see selected-keymap
         ; ("C-c m p" . mc/mark-previous-like-this-word)    ; extraneous; see selected-keymap
         ("C-c m m" . mc/mark-all-dwim)              ; can also start this with C-SPC m
         )
  :bind (:map mc/keymap ("RET" . nil))  ; Don't abort mc mode; pass thru to selected-keymap.
  :bind (:map selected-keymap
              ;; editing consecutive lines
              ("l" . mc/edit-lines)
              ("^" . mc/edit-beginnings-of-lines)
              ("$" . mc/edit-ends-of-lines)
              ;; searching for text
              ("a" . mc/mark-all-like-this)
              ("m" . mc/mark-all-dwim)                 ; can press repeatedly to mark outward
              ("n" . mc/mark-next-like-this)
              ("p" . mc/unmark-next-like-this)
              ("P" . mc/mark-previous-like-this)
              ("N" . mc/unmark-previous-like-this)
              ("." . mc/skip-to-next-like-this)
              ("," . mc/skip-to-prev-like-this)
              ("h" . mc-hide-unmatched-lines-mode)
              ("[" . mc/cycle-backward)                ; focus on prev/next cursor
              ("]" . mc/cycle-forward)
              ;; utilities
              ("\\" . mc/vertical-align-with-space)    ; place cursors on char to align
              ("#" . mc/insert-numbers) ; use num prefix to set the starting number
              ))

;; https://github.com/zk-phi/phi-search works with multiple-cursors and lets you exit
;; isearch at beginning of match, much better than end.
;; But I can't recommend it because:
;; - if match not found on a line, adds spurious cursors to beginning of region
;; - phi-replace-query crashes
;; (use-package phi-search
;;   :bind (("C-s" . phi-search)           ;; note! all search/replace is regexp based
;;          ("C-r" . phi-search-backward)
;;          ("M-%" . phi-replace-query)
;;          :map phi-search-default-map
;;          ;; Make RET complete at beginning of match, instead of end of match like isearch.
;;          ;; Original behavior available on M-RET.
;;          ("RET" . phi-search-complete-at-beginning)
;;          ("M-RET" . phi-search-complete) ; can't type C-RET on tty
;;          ))

;; https://github.com/lewang/jump-char
;; Press char multiple times to keep jumping, or use , and ; to move.
;; Press any other key to type, or RET to exit.
;; This has a decent interface but is not compatible with multiple-cursors.
;; https://github.com/doitian/iy-go-to-char may work with multiple cursors but looks
;; unsupported. You may want to try implementing your own jump-char which just does
;; zap-{up-}to-char without the zap. (Prefix would be # of forward or backwards chars to search.)
(use-package jump-char
  :bind ("M-m" . jump-char-forward)
  :bind ("M-M" . jump-char-backward)
  ;; Behave like isearch and make RET exit search.
  :bind (:map jump-char-base-map ("RET" . jump-char-exit)))

(provide 'init-mc)
