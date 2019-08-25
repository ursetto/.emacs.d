;; For unknown reason, many of the font faces contained (min-colors
;; 88) which displays just "white" on an Apple terminal.  I deleted
;; these by hand.

;; Also the colors require the bold attribute to display bright
;; enough in a terminal--this could screw up Aquamacs if set.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-overstrike-face ((((background dark)) (:foreground "#ff3"))))
 '(Man-underline-face (quote woman-italic-face))
 '(TeX-PDF-mode t)
 '(ahg-global-key-prefix "g")
 '(blink-cursor-mode t nil (frame))
 '(bs-attributes-list
   (quote
    (("" 1 1 left bs--get-marked-string)
     ("M" 1 1 left bs--get-modified-string)
     ("R" 2 2 left bs--get-readonly-string)
     ("Buffer" bs--get-name-length 10 left bs--get-name)
     ("" 1 1 left " ")
     ("Size" 8 8 right bs--get-size-string)
     ("" 1 1 left " ")
     ("Mode" 10 10 right bs--get-mode-name)
     ("" 2 2 left "  ")
     ("File" 18 18 left bs--get-file-name-abbrev)
     ("" 2 2 left "  "))))
 '(bs-configurations
   (quote
    (("all" nil nil nil nil nil)
     ("files" "^\\*scheme\\*$\\|^\\*info\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last))))
 '(bs-default-sort-name "by filename")
 '(bs-max-window-height 50)
 '(bs-sort-functions
   (quote
    (("by name" bs--sort-by-name "Buffer" region)
     ("by mode" bs--sort-by-mode-stable "Mode" region)
     ("by filename" bs--sort-by-filename-stable-empty-end "File" region))))
 '(cperl-invalid-face nil)
 '(filesets-data
   (quote
    (("main"
      (:files "/Users/jim/doc/to-read.txt" "/Users/jim/.emacs")))))
 '(fringe-indicators (quote empty) nil (fringe))
 '(fringe-mode (quote (1 . 8)) nil (fringe))
 '(glasses-face (quote bold))
 '(glasses-separator "")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(highlight-nonselected-windows t)
 '(indicate-buffer-boundaries (quote right))
 '(jit-lock-context-time 0.2)
 '(large-file-warning-threshold nil)
 '(org-agenda-files
   (quote
    ("~/business/forms-and-payments.txt" "~/doc/to-read.txt" "~/doc/OSX.txt" "~/doc/todo.txt")))
 '(package-selected-packages
   (quote
    (undo-tree indent-tools hydra ace-window use-package avy esup cargo company-quickhelp company company-ghci merlin auto-complete tuareg utop smex rust-mode org magit ido-ubiquitous idle-highlight-mode haskell-mode erlang csv-mode ahg)))
 '(quack-default-program "\"~/scheme/xcode/Currency Converter/runapp\"")
 '(quack-fontify-style (quote plt))
 '(quack-pretty-lambda-p t)
 '(quack-programs
   (quote
    ("~/local/chicken/5.0.0/bin/csi" "~/local/chicken-4.12.0/bin/csi" "~/local/chicken/4.8.0.6/bin/csi" "~/local/chicken-4.x/bin/csi" "\"~/scheme/xcode/Currency Converter/runapp\"" "./csi" "/Users/jim/scheme/sdl/repl" "/Users/jim/tmp/sdl-gears" "/usr/bin/perl -d -e2" "/usr/bin/perl -d aacraid.pl" "/usr/bin/perl -d audit-all.pl" "/usr/bin/perl -d dmi.pl" "/usr/local/bin/csi" "/usr/local/bin/csi -:a20" "/usr/local/bin/csi -:a25" "/usr/local/bin/csi -i" "/usr/local/bin/ghci" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi" "~/dl/chicken-1.88/csi" "~/local/bin/csi" "~/local/bin/csi " "~/local/bin/csi -:d" "~/local/bin/csi -:d -:f32" "~/local/bin/csi -:d -:f4" "~/local/bin/csi -:d -:f8192" "~/local/bin/csi -:f16384" "~/local/bin/csi -:f2500" "~/local/bin/csi -:f3 -:d" "~/local/bin/csi -:f5600 -:d" "~/local/bin/csi -:f5700 -:d" "~/local/bin/csi -:f6000" "~/local/bin/csi -:f6000 -:d" "~/local/bin/csi -:f8192" "~/local/bin/csi -no-init" "~/local/bin/csi -syntax" "~/local/bin/csi4 -no-init -:a25" "~/local/bin/ghci" "~/local/chicken-4.6.6/bin/csi" "~/local/chicken-4.6/bin/csi" "~/local/chicken-4.7.0-st-clang/bin/csi" "~/local/chicken-4.7.0-st/bin/csi" "~/local/chicken-4.8/bin/csi" "~/local/chicken-4/bin/csi" "~/local/chicken-hygienic/bin/csi -:a20" "~/local/chicken/4.8.0.1/bin/csi" "~/local/chicken/4.x/bin/csi" "~/scheme/chicken-core/inst/bin/csi" "~/scheme/xcode" "~/scheme/xcode/Currency Converter/runapp" "~/scheme/xcode/CurrencyConverter/runapp" "~/scheme/xcode/Currency\\ Converter/runapp" "~/scheme/xcode/color-view/runapp")))
 '(quack-remap-find-file-bindings-p nil)
 '(quack-run-scheme-always-prompts-p t)
 '(safe-local-variable-values
   (quote
    ((org-agenda-show-all-dates)
     (org-agenda-todo-ignore-scheduled . all)
     (org-cycle-include-plain-lists . integrate)
     (outline-cycle-min-level . 4)
     (outline-cycle-min-level . 2))))
 '(tool-bar-mode nil)
 '(tramp-debug-buffer nil)
 '(tramp-methods
   (quote
    (("smb")
     ("ftp")
     ("rcp"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "rsh")
      (tramp-copy-program "rcp")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args nil)
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("scp"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "/usr/local/bin/ssh")
      (tramp-copy-program "/usr/local/bin/scp")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("scp1"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh")
      (tramp-copy-program "scp")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-1" "-e" "none"))
      (tramp-copy-args
       ("-1"))
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("scp2"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh")
      (tramp-copy-program "scp")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-2" "-e" "none"))
      (tramp-copy-args
       ("-2"))
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("scp1_old"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh1")
      (tramp-copy-program "scp1")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("scp2_old"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh2")
      (tramp-copy-program "scp2")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("rsync"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh")
      (tramp-copy-program "rsync")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none"))
      (tramp-copy-args
       ("-e" "ssh"))
      (tramp-copy-keep-date-arg "-t")
      (tramp-password-end-of-line nil))
     ("remcp"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "remsh")
      (tramp-copy-program "rcp")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args nil)
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("rsh"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "rsh")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args nil)
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("ssh"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "/usr/local/bin/ssh")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("ssh1"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-1" "-e" "none"))
      (tramp-copy-args
       ("-1"))
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("ssh2"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-2" "-e" "none"))
      (tramp-copy-args
       ("-2"))
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("ssh1_old"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh1")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("ssh2_old"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh2")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("remsh"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "remsh")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args nil)
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("telnet"
      (tramp-connection-function tramp-open-connection-telnet)
      (tramp-login-program "telnet")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args nil)
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("su"
      (tramp-connection-function tramp-open-connection-su)
      (tramp-login-program "su")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-" "%u"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("sudo"
      (tramp-connection-function tramp-open-connection-su)
      (tramp-login-program "sudo")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-u" "%u" "-s" "-p" "Password:"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("multi"
      (tramp-connection-function tramp-open-connection-multi)
      (tramp-login-program nil)
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args nil)
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("scpx"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh")
      (tramp-copy-program "scp")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none" "-t" "-t" "/bin/sh"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil))
     ("sshx"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "ssh")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-e" "none" "-t" "-t" "/bin/sh"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("krlogin"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "krlogin")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-x"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line nil))
     ("plink"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "plink")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-ssh"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line "xy"))
     ("plink1"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "plink")
      (tramp-copy-program nil)
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-1" "-ssh"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg nil)
      (tramp-password-end-of-line "xy"))
     ("pscp"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "plink")
      (tramp-copy-program "pscp")
      (tramp-remote-sh "/bin/sh")
      (tramp-login-args
       ("-ssh"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line "xy"))
     ("fcp"
      (tramp-connection-function tramp-open-connection-rsh)
      (tramp-login-program "fsh")
      (tramp-copy-program "fcp")
      (tramp-remote-sh "/bin/sh -i")
      (tramp-login-args
       ("sh" "-i"))
      (tramp-copy-args nil)
      (tramp-copy-keep-date-arg "-p")
      (tramp-password-end-of-line nil)))) t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PT Mono" :height 100 :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
 '(avy-lead-face ((t (:background "color-124" :foreground "white"))))
 '(avy-lead-face-0 ((t (:background "#e52b50" :foreground "white"))))
 '(avy-lead-face-1 ((t (:background "color-240" :foreground "white"))))
 '(avy-lead-face-2 ((t (:background "color-127" :foreground "white"))))
 '(bold ((t (:underline "cyan"))))
 '(caml-types-expr-face ((t (:background "#448822"))))
 '(comint-highlight-input ((t (:foreground "#0090ff"))))
 '(company-echo-common ((t (:foreground "brightyellow"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "gray70"))))
 '(company-scrollbar-bg ((t (:background "darkcyan"))))
 '(company-scrollbar-fg ((t (:background "gray30"))))
 '(company-tooltip ((t (:background "#001" :foreground "gray70"))))
 '(company-tooltip-annotation ((t (:foreground "cyan"))))
 '(company-tooltip-common ((t (:foreground "brightyellow"))))
 '(company-tooltip-selection ((t (:background "darkblue"))))
 '(cperl-array ((((class color) (background dark)) (:foreground "tan"))))
 '(cperl-array-face ((((class color) (background dark)) (:foreground "steelblue3"))))
 '(cperl-hash ((((class color) (background dark)) (:foreground "#ff4060"))))
 '(cperl-hash-face ((((class color) (background dark)) (:foreground "#f080c0"))))
 '(cperl-nonoverridable-face ((((class color) (background dark)) (:foreground "#e080ff"))))
 '(cursor ((t (:background "#99b"))))
 '(diff-added ((t (:inherit diff-changed :foreground "brightgreen"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "brightred"))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "#ddaaff"))))
 '(font-lock-comment-face ((((class color) (background dark)) (:foreground "#ff6020"))))
 '(font-lock-doc-face ((t (:foreground "#ff8050"))))
 '(font-lock-keyword-face ((((class color) (background dark)) (:foreground "#ffff30"))))
 '(font-lock-string-face ((((class color) (background dark)) (:foreground "#00ff30"))))
 '(font-lock-type-face ((((class color) (background dark)) (:foreground "lightsteelblue"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "#00d0ff" :underline nil))))
 '(fringe ((((class color) (background dark)) (:background "#383838" :foreground "#aaaaaa"))))
 '(hl-line ((t (:inverse-video t))))
 '(ido-only-match ((((class color)) (:foreground "Green"))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "yellow1"))))
 '(info-menu-header ((t (:inherit variable-pitch))))
 '(info-title-4 ((t (:inherit variable-pitch))))
 '(info-xref ((((class color) (background dark)) (:foreground "steelblue1"))))
 '(info-xref-visited ((default (:inherit info-xref)) (((class color) (background dark)) (:foreground "#e070ff"))))
 '(italic ((((supports :slant italic)) (:underline "green"))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "green"))))
 '(magit-diff-added ((t (:background "#004411" :foreground "#ccddcc"))))
 '(magit-diff-added-highlight ((t (:background "#114411" :foreground "#eeffee"))))
 '(magit-diff-context-highlight ((t (:background "grey10" :foreground "grey70"))))
 '(magit-diff-file-header ((t (:foreground "brightmagenta"))))
 '(magit-diff-hunk-header ((t (:foreground "cyan" :slant italic))))
 '(magit-diff-hunk-heading-highlight ((t (:background "grey35" :foreground "grey90"))))
 '(magit-diff-removed ((t (:background "#441111" :foreground "#ddbbbb"))))
 '(magit-diff-removed-highlight ((t (:background "#661111" :foreground "#ffdddd"))))
 '(magit-hash ((t (:foreground "color-146"))))
 '(magit-item-highlight ((t (:background "#00005f"))))
 '(magit-section-highlight ((t (:background "color-235"))))
 '(mode-line ((nil (:background "#000040" :foreground "white"))))
 '(mode-line-inactive ((t (:background "#000040" :foreground "gray50"))))
 '(org-done ((t (:foreground "green" :weight normal))))
 '(org-level-2 ((((class color) (background dark)) (:inherit font-lock-variable-name-face))))
 '(org-level-3 ((((class color) (background dark)) (:inherit font-lock-keyword-face))))
 '(org-level-4 ((((class color) (background dark)) (:inherit font-lock-builtin-face))))
 '(org-level-5 ((((class color) (background dark)) (:inherit font-lock-constant-face))))
 '(org-level-6 ((((class color) (background dark)) (:inherit font-lock-type-face))))
 '(org-level-7 ((((class color) (background dark)) (:inherit font-lock-string-face))))
 '(org-level-8 ((((class color) (background dark)) (:inherit font-lock-comment-face))))
 '(org-link ((((class color) (background dark)) (:foreground "#007fff" :underline t :weight bold))))
 '(org-tag ((t (:background "blue" :foreground "white"))))
 '(org-todo ((t (:foreground "Red" :weight bold))))
 '(quack-pltish-defn-face ((t (:foreground "#87d7ff"))))
 '(quack-pltish-keyword-face ((t (:foreground "brightyellow"))))
 '(quack-pltish-paren-face ((((class color) (background dark)) (:foreground "#c00000"))))
 '(secondary-selection ((t (:background "#303030"))))
 '(show-paren-match ((((class color) (background dark)) (:background "#ff5020"))))
 '(vhdl-font-lock-reserved-words-face ((t (:foreground "blue" :weight bold))))
 '(woman-bold-face ((((background dark)) (:foreground "#ff3"))))
 '(woman-italic-face ((((background dark)) (:foreground "#5cf")))))
