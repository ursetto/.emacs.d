;; early-init's main purpose is to set up the package manager.  In Emacs >= 27, that's
;; done implicitly at the end of early-init, but in earlier versions it happens at the end
;; of init, so do it manually now. This can also be deferred to early in init.el, but take
;; care to disable the implicit call here.

(setq package-enable-at-startup nil) ;; Prevent package-initialize from being called implicitly.
(setq package-quickstart t)  ;; (ignored on Emacs < 27) M-x package-quickstart-refresh rebuilds package-quickstart.el.
(setq package-archives '())  ;; Saves a lot of time in package-initialize, skips slow archive file read.
(package-initialize)
(setq package-archives       ;; Now set the archives so later installs work.
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;;("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
;; Note: Setting package-archives correctly after init still allows init-time install of
;; missing packages, but you get a warning that package-initialize is called twice.
;; Probably harmless and will not recur after you restart. So far, no ill effects from
;; setting archives to '() upfront.

;; Pull in use-package as we rely on it heavily.
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)  ; M-x use-package-report
(use-package diminish)
