;;;; haskell
;; Use M-x run-haskell to start GHCi.
;; Use C-c C-l to load the file into a GHCi process so company-mode can offer completions in code. (Use M-/ to complete using plain dabbrev if the code is not loaded.)
;; Use C-c C-i to get information on the identifier at point.
;; Use C-c C-c to compile.

;(add-hook 'haskell-mode-hook 'haskell-doc-mode)       ; Slows down company-mode a lot if active.
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent) ; turn-on-haskell-indentation?  may be automatic after 2.7

;; --- Set up company-mode for Haskell. We assume company-mode is autoloaded elsewhere.
;; --- "Intero" is supposed to be a more modern IDE mode for Haskell, but is untested.

;; Note these caveats:
;; - company-mode and haskell-doc-mode have a bad interaction that will freeze the cursor while
;;   completions are going on. Avoiding haskell-doc-mode seems to fix it. You still get type signatures
;;   in the echo area, even when company isn't running, and I don't know why.

(use-package company-ghci)

(use-package haskell-mode
  :bind
  (:map interactive-haskell-mode-map
        ;; Normally 'haskell-process-cabal-build. Remove if we start using Cabal.
        ;; Use C-u C-c C-c to change the compilation command; you can remove -c to build an executable.
        ;; (Then press 'g' in the compilation buffer to rebuild with same options.)
        ("C-c C-c" . haskell-compile))
  :hook
  ((haskell-mode . (lambda ()
                     (auto-complete-mode 0)         ; interferes with company-mode
                     (setq-local company-backends (cons 'company-ghci company-backends))
                     (company-mode)
                     (interactive-haskell-mode)     ; So company-mode can do completions
                     (setq haskell-process-use-presentation-mode t)  ; So C-c C-i opens a buffer instead of the echo area
                     ;; Correct for bug #1553 in haskell-mode (GHC 8.2.2 - 8.4.3+) which prevented C-c C-l
                     ;; from reporting errors properly, which still exists as of 2018-08-27. Untested is
                     ;; whether the custom ghci provided with Intero works around this itself.
                     ;; https://github.com/haskell/haskell-mode/issues/1553#issuecomment-358373643
                     (setq haskell-process-args-ghci
                           '("-ferror-spans" "-fshow-loaded-modules"))
                     (setq haskell-process-args-cabal-repl
                           '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
                     (setq haskell-process-args-stack-ghci
                           '("--ghci-options=-ferror-spans -fshow-loaded-modules"
                             "--no-build" "--no-load"))
                     (setq haskell-process-args-cabal-new-repl
                           '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
                     ))
   (haskell-interactive-mode-hook . company-mode))
  )

(provide 'init-haskell)
