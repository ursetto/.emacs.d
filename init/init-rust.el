;;;; rust-mode

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
  (setq lsp-keymap-prefix "M-l")  ;; overrides downcase-word
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :hook (rust-mode . (lambda ()
                       ;; Git VC backend lets lsp find our project root. Enable it in rust+lsp buffers,
                       ;; as generic VC mode is usually disabled globally for performance.
                       (if (null vc-handled-backends)
                           (setq-local vc-handled-backends '(Git)))
                       (lsp))))

(provide 'init-rust)
