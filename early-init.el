;; Temporarily changing gc size and nulling handler list during startup
;; saves ~100ms in startup costs.
(defvar gc-cons-threshold--orig gc-cons-threshold)
(defvar file-name-handler-alist--orig file-name-handler-alist)
(setq gc-cons-threshold 100000000
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook      ; Reset these after we are done
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold--orig
                  file-name-handler-alist file-name-handler-alist--orig)))

;; early-init's main purpose is to set up the package manager.  In Emacs >= 27, that's
;; done implicitly at the end of early-init, but in earlier versions it happens at the end
;; of init. We'll start it ourselves later.

(setq package-enable-at-startup nil) ;; Prevent package-initialize from being called implicitly.
