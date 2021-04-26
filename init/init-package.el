;; Initialize straight.el package manager.

;; straight assumes you may manually edit or pull package files, and tries to detect this
;; at startup. This is expensive and unlikely, so remove 'find-at-startup from the
;; modification list. Use M-x straight-check-all to recheck when needed, or see:
;; https://github.com/raxod502/straight.el#customizing-when-packages-are-built
(setq straight-check-for-modifications '(find-when-checking))

;; Standard bootstrap preamble from https://github.com/raxod502/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)  ; M-x use-package-report
(use-package diminish)

(provide 'init-package)
