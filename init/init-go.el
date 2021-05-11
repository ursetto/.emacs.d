;;; setup-go.el -*- lexical-binding: t; -*-

;; manual actions:
;; Add ~/go/bin to PATH
;; go get golang.org/x/tools/gopls
;; go get github.com/rogpeppe/godef

(use-package go-mode
  :defer t
  :hook ((go-mode . (lambda ()
                      ;(lsp-deferred)
                      (setq-local indent-tabs-mode t) ;; are you kidding me?
                      (setq tab-width 4)
                      (set (make-local-variable 'company-backends)
                           '((company-capf company-files :with company-yasnippet)
                             (company-dabbrev-code company-dabbrev)))
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t)
                      ;(company-mode) ; this is implicit
                      (lsp)))))

(provide 'init-go)
