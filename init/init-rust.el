;;;; rust-mode

;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
;;
;; Obtain rust-analyzer binary with (using '-linux' or '-mac' as needed):
;;   curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer
;;   chmod +x ~/.local/bin/rust-analyzer
;;
;; - rust-analyzer does not work well with lsp. The main problem is it won't show function signatures
;; in the minibuffer, because the signature is not on the first line of the doc (it's
;; usually the crate path, which who cares). If you (setq lsp-eldoc-render-all t) to show all docs,
;; the minibuffer now contains *too much* info and scrolls the signature off.  See
;; https://github.com/emacs-lsp/lsp-mode/issues/2613. rls just works, even when the signature
;; spans multiple lines. If eldoc could limit its output to a few lines (at the beginning!)
;; it would be ok.
;; - rust-analyzer says 'unknown' for types far too often.
;; - rls uses much more disk space, though starts up faster.
;; - In lsp-ui-doc mode, rust-analyzer doc basically works, but needs a wide terminal
;;   or your code gets obscured.

;; TODO: If *lsp-help* buffer is open in a window (M-l h h), try to get documentation dynamically
;; updated there, instead of using the lsp-ui-doc overlay.

;; Note: If you skip importing a project root ("d"), and later want to import it, use
;;       M-x lsp-workspace-blacklist-remove. State is stored in ~/.emacs.d/.lsp-session-v1.
;;
;; Note: It might be nice to avoid activating lsp for a workspace automatically, because it may compile
;;       hundreds of megs of code. Once the server is running though, it can be enabled without penalty.
;;       Putting projects on the blacklist (maybe temporarily) is not a great solution.
;;       But the lsp backend code is not set up for this (or I can't understand it enough).
;;       As a workaround, we avoid starting lsp-mode until it has been explicitly invoked once via M-x lsp.
;;       - Also note: `M-l s s` is bound to `lsp`, but pointless (?)--can't use until lsp is active in buffer.

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package rust-mode
  :defer t)

(use-package flycheck
  :defer t
  :config
  ;; Use C-c ! l to display error list.
  (add-to-list 'display-buffer-alist
               ;; Always display flycheck window in the bottom 10% of the screen.
               ;; This comes from the manual (https://www.flycheck.org/en/latest/user/error-list.html#tune-error-list-display)
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.10)))   ;; use float for %age, integer for fixed # of lines
  ;; (list-flycheck-errors)   ; does not work to display on flycheck startup
  )

(use-package flycheck-rust  ;; So flycheck understands the cargo workspace.
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package lsp-mode
  :commands lsp lsp-deferred
  :init
  (setq lsp-keymap-prefix "M-l")  ;; overrides downcase-word.  Note: show doc at point with M-l h h.
  :custom
  (lsp-eldoc-render-all nil)  ;; Render all is bad when doc is long.
  ; (lsp-rust-analyzer-server-display-inlay-hints t)   ;; inline type annotations -- does not work until 2nd file is loaded
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-server 'rust-analyzer) ;; rust-analyzer or rls
  (lsp-enable-symbol-highlighting nil) ;; Activate with M-l a h
  (read-process-output-max (* 1024 1024))
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :hook (rust-mode . (lambda ()
                       ;; Git VC backend lets lsp find our project root. Enable it in rust+lsp buffers,
                       ;; as generic VC mode is usually disabled globally for performance.
                       (if (null vc-handled-backends)
                           (setq-local vc-handled-backends '(Git)))
                       ;; Avoid invoking lsp until the user explicitly has done it once via `M-x lsp`.
                       ;; Not a perfect solution--would prefer a per-*workspace* activation--but this does
                       ;; prevent starting up by accident when you are just browsing.
                       (when (fboundp 'lsp-mode)
                         (lsp))
                       )))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)  ; toggle with M-l T d
  (lsp-ui-doc-include-signature t) ; Include signature for rls. Has no effect for rust-analyzer.
  (lsp-ui-doc-max-height 25)
  (lsp-ui-sideline-show-hover nil)  ; Semi-useful but cluttered. With rust-analyzer, often spams screen with doc text.
  (lsp-ui-sideline-show-code-actions nil)
  )

(provide 'init-rust)
