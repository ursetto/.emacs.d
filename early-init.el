;; early-init's main purpose is to set up the package manager.  In Emacs >= 27, that's
;; done implicitly at the end of early-init, but in earlier versions it happens at the end
;; of init, so do it manually now. This can also be deferred to early in init.el, but take
;; care to disable the implicit call here.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;;("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil) ;; Prevent package-initialize from being called implicitly.
(setq package-quickstart t)  ;; (ignored on Emacs < 27) M-x package-quickstart-refresh rebuilds package-quickstart.el.
(package-initialize)

;; Also pull in 'use-package as we rely on it heavily.

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)  ; M-x use-package-report
(use-package diminish)
