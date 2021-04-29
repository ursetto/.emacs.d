;;; init.el
;; Invoke early-init in versions that don't do so automatically.
(when (version< emacs-version "27")
  (load (locate-user-emacs-file "early-init.el")))

(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message ";; Scratch buffer\n\n") ;; For elisp, use M-x ielm or M-x lisp-interaction-mode

;; Local, non-packaged software. Customizations may immediately require
;; local features (session-use-package is an example) so do this first.
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
;; My init code.
(add-to-list 'load-path (locate-user-emacs-file "init"))

;; Load customization file at start. If we load at end of file, any implicit updates to
;; customized variables here are lost — it seems they are not written to disk immediately.
;; In particular, use-package may modify package-selected-packages (via package-install);
;; this update would be lost on load, causing package-autoremove to get out of sync.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load-file custom-file)

(require 'init-package)  ;; Initialize package manager.

(use-package filladapt :commands filladapt-mode)
(autoload 'zap-up-to-char "misc" "Load this for zap-up-to-char" t)

(use-package hydra)

(use-package avy
  :bind
  (("C-c '" . avy-goto-char-2)
   ("C-c /" . avy-goto-char-timer)   ;; conflict?
   ("M-g M-g" . avy-goto-line)
   ("M-g [" . avy-goto-paren-open)
   ("M-g ]" . avy-goto-paren-close))
  :config
  (setq avy-style 'at-full)
  (setq avy-styles-alist '((avy-goto-line . pre)))

  (setq avy-indent-line-overlay t)
  ;; (setq avy-all-windows nil)

  (defun avy-goto-paren-open ()
    (interactive)
    (avy--generic-jump "(\\|{\\|\\[" nil 'pre))

  (defun avy-goto-paren-close ()
    (interactive)
    (avy--generic-jump ")\\|}\\|]" nil 'pre)))

;;;; magit
;; Note: global-magit-file-mode is turned on by default, so these are
;; automatically bound to C-x g, C-x M-g, and C-c M-g (magit-file-popup)
;; in all file-visiting buffers. We should probably switch to those bindings,
;; though Ctrl->Meta is torturous to type and could be changed in magit-file-mode-map.
(use-package magit
  :bind (("C-x g" . magit-status)                 ;; was: C-c m
         ("C-x M-g" . magit-dispatch-popup)       ;; was: C-c C-m
         (:map magit-status-mode-map
               ;("<backtab>" . magit-section-cycle-global)  ;; S-<tab> ok by default
               ("C-M-i" . magit-section-cycle-diffs)   ;; M-<tab> rebind
               ("C-c TAB" . magit-section-cycle))))    ;; C-<tab> unreachable on TTY
;; Disable all VC mode backends, and use dedicated packages like magit for
;; version control. This speeds up tramp remote operations. This may have unexpected
;; consequences -- for example, project.el uses the VC backend to find the project root by
;; default. In particular, disabling Git can prevent lsp and elgot from guessing the project root,
;; so you can can reenable it in a mode hook for those.
(setq vc-handled-backends nil)

(use-package indent-tools
  :bind (("C-c >" . indent-tools-hydra/body))
  :config (use-package s)  ;; Required for up/down motion; not autoloaded
  )
(use-package undo-tree
  ;; C-x u to visualize; p/n/b/f to navigate; d to diff; t for timestamps; C-q to abort; M-_ to redo
  ;; Also see ~/.emacs.d/elpa/undo-tree-readme.txt
  :diminish
  :defer 1
  :config (global-undo-tree-mode))

;;;; session -- save command and variable history across sessions
;; session can't be downloaded from MELPA because it has old, buggy version 2.3a.
;; I've submitted a PR for version 2.4b (https://github.com/emacsorphanage/session/pull/2)
;; but it may be wise to migrate to a maintained package. Meantime, use local copy in lisp/.
;; Note: session-initialize writes a custom var which requires feature 'session.
(use-package session
  :ensure nil   ; don't grab from melpa
  :straight nil
  :init (add-hook 'after-init-hook 'session-initialize))

(use-package edit-indirect
  ;; C-c C-c : commit     C-c C-k : abort     C-x C-s : update and continue
  ;; To find existing buffer, select entire region and edit again. Consider implementing
  ;; ability to autoselect region for editing when inside existing one.
  :bind (("C-x n e" . edit-indirect-region)))

(use-package which-key    ;; Display popup key bindings. Globally useful.
  :defer 1
  :diminish
  :config
  (which-key-mode t))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-x 4 t" . crux-transpose-windows) ; only works intuitively with 2 windows
         ("C-c R" . crux-rename-file-and-buffer)
         ("C-c D" . crux-delete-file-and-buffer)
         ;;("C-k" . crux-smart-kill-line) ; inside line kills to EOL, again kills whole line
         ("C-k" . crux-kill-and-join-forward) ; inside line kill to EOL, again joins next line (like C-k C-^)
         ("C-c DEL" . crux-kill-line-backwards) ; can't do C-DEL on tty
         ("C-c K" . crux-kill-other-buffers)
         ("M-k" . crux-kill-whole-line) ; Dedicated key is easier and more reliable than C-a C-a C-k.
         ("C-^" . crux-top-join-line)   ; Like M-^, but join next line to this one. M-j (really Super-J) works too.
         ("M-j" . crux-top-join-line)   ; (Testing.) This overrides default-indent-new-line (which I don't use).
                                        ; C-k at EOL does a join already, except in paredit mode.
         )
  :init
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer)
  :defer 1   ; Due to the advice in :config, this can't be deferred indefinitely.
  :config
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region))

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h M-m" . discover-my-mode)))
(use-package idle-highlight-mode :defer t)

;;; Initialization
;;;; Main

(menu-bar-mode 0)
(set-language-environment "UTF-8")
(global-font-lock-mode 1)
(setq echo-keystrokes 0.1)
(icomplete-mode 1)              ;; Completion of non-ido things like C-h v, C-h f
;; (iswitchb-mode t)            ;; Switch between buffers using substrings (using ido-mode instead)
;; Ignore these extensions during filename completion (works with ido and others).
(add-to-list 'completion-ignored-extensions "~/")
(add-to-list 'completion-ignored-extensions ".retry")
(add-to-list 'completion-ignored-extensions "__pycache__/")
(winner-mode t)                         ;; use C-c left/right to undo/redo window config
(when (eq window-system 'mac)   ;; Only in OS X GUI mode.  Not a great solution.
  (server-start)                          ;; for emacsclient
  )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(show-paren-mode t)
(setq show-paren-delay 0)
;(setq kill-whole-line 1)   ; C-k at beginning of line kills entire line. Works with crux. Prefer M-k.
(setq-default show-trailing-whitespace t)
(setq backup-directory-alist (cons (cons "." ".~")  ;; All backup files saved to .~/
                                   backup-directory-alist))
(setq require-final-newline t) ;; Add final newline when not present (set to 'query to ask first)
(setq-default indent-tabs-mode nil)    ;; screw it, tab characters are dumb.
(setq enable-recursive-minibuffers t)
(transient-mark-mode 1)
(setq-default mark-even-if-inactive nil)  ; nil: Region commands don't operate on disappeared highlighting
;;;; Show line/column like (30,2)
(line-number-mode 1)
(column-number-mode 1)

;;;; Keybindings
(bind-key "C-c ;" 'comment-or-uncomment-region)  ;; note M-; is comment-dwim
(bind-key "C-x C-a" 'auto-fill-mode)   ;; Another option is refill-mode.
(bind-key "C-x k" 'kill-this-buffer)   ;; Don't ask for which buffer to kill every time
(bind-key "C-c z" 'zap-up-to-char)     ;; requires 'misc
(bind-key "C-c C-j" 'imenu)            ;; jump to definition in this file (python binding; also used for org-goto)
(bind-key "C-c b" (lambda () (interactive) (switch-to-buffer nil)))  ;; switch to previous buffer

(require 'init-window)
(require 'init-mc)
;;(require 'init-fontset)         ;; legacy, unused

;;;; Aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'ts 'tags-search)
(defalias 'ta 'tags-apropos)
(defalias 'ws 'whitespace-mode)
(defalias 'wst 'whitespace-toggle-options)
(defalias 'tail-mode 'auto-revert-tail-mode)

;;;; Terminal

;(unless window-system
 ;(xterm-mouse-mode 1)
; (defun track-mouse (arg1))  ;; No idea, but complains it is void otherwise.
;)

;;;; Use hippie-expand for M-/
(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))
;; At the moment I have simply moved -dabbrev and dabbrev-all-buffers ahead of
;; -list and -line.  See http://www.xemacs.org/Documentation/packages/html/edit-utils_23.html.
(setq hippie-expand-try-functions-list
 '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs
   try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-list try-expand-line
   try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;;;; Misc

;; My replacement for slowsplit.el which works better with nonzero
;; line-spacing, though it craps out if point is
;; more than halfway down the screen.
(defun split-window-quietly-zb (&optional arg)
  "Split the window vertically with minimum redisplay.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally.  Does not
work properly when arg given."
  (interactive "P")
  (let* ((num-arg (and arg (prefix-numeric-value arg)))
         (oldpt (point)))
    (split-window nil num-arg)
    (move-to-window-line -1)  ;; can't use window-end, not updated yet
    (let ((end (point)))
      (other-window 1)
      (goto-char end)
      (recenter 0)
      (other-window -1)
      (goto-char oldpt)
)))
(bind-key "C-x 2" 'split-window-quietly-zb)

;; "Copy rectangle" from http://www.emacswiki.org/emacs/RectangleCommands
(defun my-copy-rectangle (start end)
  "Copy the region-rectangle instead of `kill-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
(bind-key "C-x r M-w" 'my-copy-rectangle)

(defun delete-file-and-buffer (&optional no-confirm-p)
  "Delete the file associated with the current buffer, and kill the buffer.
If it is not a file buffer, nothing happens.

When no-confirm-p is t (or called with C-u), does not ask for confirmation."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (if filename
        (when (or no-confirm-p
                  (y-or-n-p (concat "Delete " filename)))
          (progn
            (delete-file filename)
            (message "Deleted %s" filename)
            (kill-buffer)))
      (message "No file associated with this buffer"))))
(bind-key "C-x C-d" 'delete-file-and-buffer)   ;; override list-directory

;; Indent-rigidly doesn't insert space in blank lines.
(defun indent-rigidly-even-when-blank (start end arg)
  "Indent all lines starting in the region sideways by ARG columns,
even when the line is blank.  Modified from indent-rigidly in indent.el.
Called from a program, takes three arguments, START, END and ARG.
You can remove all indentation from a region by giving a large negative ARG."
  (interactive "r\np")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (point) end)
      (let ((indent (current-indentation))
            eol-flag)
        (save-excursion
          (skip-chars-forward " \t")
          (setq eol-flag (eolp)))
        (or ;eol-flag                 ;; the only change
            (indent-to (max 0 (+ indent arg)) 0))
        (delete-region (point) (progn (skip-chars-forward " \t") (point))))
      (forward-line 1))
    (move-marker end nil)))
;; Globally set this (even in other modes). Indented blank lines may
;; not be a good idea anymore, since it counts as trailing whitespace,
;; so it is disabled.
; (bind-key "C-x <tab>" 'indent-rigidly-even-when-blank)

;;; Modes


(require 'init-outline)
(require 'init-org)

;;;;;; Selective display (outline-mode and org-mode invisible text ellipsis)

;; display-table-slot for selective-display is a vector
;; of glyphs; a glyph is a char plus a face.  See info:Display Tables.
(make-face 'invisible-text-ellipsis-face)  ;; an arbitrary name of my choosing
(set-face-foreground 'invisible-text-ellipsis-face "yellow")
(set-face-background 'invisible-text-ellipsis-face nil)
(let ((dot (make-glyph-code ?› 'invisible-text-ellipsis-face)))
  (set-display-table-slot standard-display-table 'selective-display
			  (vector ?\  dot dot dot)))

(require 'init-buffer)

;;;; ido
;; Notes: C-p (toggle between substring and prefix matching)
;;        C-s / C-r  (move forwards/backwards in completion list)
;;        RET (choose the first match; continue completion if directory)
;;        C-j (find file as is -- enters dired if complete directory name)
;;        C-SPC (lock current match and begin another)
;;        M-s (find filename in history)
;;        C-t (toggle regexp match)
;;        TAB/? (full completion list, when no completions)

(use-package ido
  :ensure nil ; not a package
  :defer 1
  :config
  (ido-mode t)                               ;; supersedes iswitchb-mode
  (setq ido-enable-tramp-completion t)
  ;; (setq ido-max-prospects 12)             ;; max # of matching items
  ;; (setq ido-show-dot-for-dired t)         ;; Interferes with last directory RET traversal --
  ;;                                         ;; C-j or C-d is better option to get into dired.
  ;; (setq ido-enable-dot-prefix t)          ;; Initial . forces prefix match. If off, can match exts
  ;;(ido-ubiquitous-mode t)                    ;; (use ido-completing-read+ now: https://benaiah.me/posts/using-ido-emacs-completion/)
  ;; ido will obey completion-ignored-extensions (when ido-ignore-extensions is t, the default).
)

;;;; smex

(use-package smex
  :bind
  (("M-x" . smex)
   ("C-c M-x" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)   ; plain vanilla M-x
   ))

;; While smex is active in the minibuffer:
;;  `C-h f` runs describe-function on the currently selected command.
;;  `M-.` jumps to the definition of the selected command.
;;  `C-h w` shows the key bindings for the selected command.

;;;;; company-mode

;; Warning: company-quickhelp does not work on tty, as pos-tip underlying library
;; (which ultimately uses built-in function x-show-tip) requires a GUI.
;; autocomplete's popup.el works fine on terminal.

(use-package company
  :bind
  ;; Use TAB to also cycle through completions instead of just completing common part.
  ;; Disallow RET from completing selection, as I often hit RET at EOL.
  ;; Move completion of selection to M-/ (M- already held if using M-n/M-p.)
  ;; The downside of M-/ is you can't easily use dabbrev or hippie-expand
  ;; when company-mode is active, since the immediate idle popup absorbs M-/.
  (:map company-active-map
        ("<tab>" . 'company-complete-common-or-cycle)
        ("C-m" . nil)
        ("M-/" . company-complete-selection))
  :config
  (setq company-idle-delay 0.2) ;; Complete fast enough to avoid need for company-complete binding
  (setq company-dabbrev-downcase nil) ;; Prevent lowercasing identifiers in comments.
  (setq company-require-match nil) ;; Allow any non-match character to break out of completion.
  ;(global-company-mode 1)   ; use company-mode in all buffers

  ;; Alternate behaviors: 1) have TAB complete selection, do not allow complete common (like Atom).
  ;;  2) have TAB complete common, then complete selection instead of cycling. Would need a custom
  ;;     function for this.
  ;; TODO: Consider raising company-idle-delay back to 0.5 and allowing completion begin on TAB, for
  ;; less popup distraction. This may impact indentation.
  )

;;;; python

(add-hook 'python-mode-hook
          (lambda ()
            (idle-highlight-mode t)
            ;; Default to Python 3 on Mac. Although "python" is correct after a `M-x pyvenv-activate`,
            ;; outside of a venv "python" means Python 2 which is usually wrong.
            (setq python-shell-interpreter "python3")))

;; `M-x elpy-config` to sanity check your config.
;; `pipx install flake8` to install python3 flake8 (it's not in the venv).
;; `M-x elpy-rpc-reinstall-virtualenv` to rebuild the venv, which is likely to break after
;; a `brew upgrade`. Don't forget `pipx reinstall-all` for flake8.
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  ;; A mixed Py2/3 environment does not work well; just use 3 now. Py2 programs
  ;; should be written to be syntactically compatible with Python 3 anyway.
  (setq elpy-rpc-python-command "python3"))

;;;; Gforth

;; forth-mode.el is gforth.el from the gforth distribution, not available as package.
;; Renamed to forth-mode.el so it provides 'forth-mode properly.
;; Recommendation: Try https://github.com/larsbrinkhoff/forth-mode from MELPA.
(use-package forth-mode
  :ensure nil  ; use local copy
  :straight nil
  :mode (("\\.fs\\'" . forth-mode)
         ("\\.fb\\'" . forth-block-mode))
  :config
  (setq forth-indent-level 4)
  (setq forth-minor-indent-level 2)
  (setq forth-hilight-level 3)
  :hook (inferior-forth-mode-hook
          . (lambda ()
              (setq comint-process-echoes t))))

;;;; csv-mode

(use-package csv-mode
  :defer t
  :config
  ;; Assume all csv files have a header, for auto region select
  (setq csv-header-lines 1))

;;;; html-mode

(defun html-tidy-buffer ()
  (interactive)
  (mark-whole-buffer)
  (shell-command-on-region (point-min) (point-max)
			   "tidy -w 0 -xml -q -i"
			   t   ; other output buffer (n/a)
			   t   ; replace
  ))

;;;; desktop

;; desktop-menu is unsupported and can only be found as a download from emacswiki.org.
;; Recommend looking for a supported package. See: https://www.emacswiki.org/emacs/SessionManagement
(use-package desktop-menu         ;; implicitly loads 'desktop
  :ensure nil   ; not a package
  :straight nil ; straight's version is extremely old
  :commands desktop-menu
  :init
  (defalias 'dm 'desktop-menu)
  :config
  (setq desktop-menu-directory "~/.emacs.d/desktops")
  (setq desktop-load-locked-desktop t)  ;; Locking unusable unless we implement desktop-menu-kill. (see below)
  (setq desktop-menu-clear t)           ;; Clear unconditionally.  'ask' is annoying and using 'x' from menu to clear beforehand also kills the menu!
  ;;(setq desktop-menu-autosave 120)    ;; autosave after # of seconds, or t to autosave at exit; default nil (no saving)

  ;; NB: customization is in customize-group desktop-menu

  ;; desktop-menu issues:
  ;;  Must press "q" to quit desktop-menu in order to save desktop list.  Otherwise
  ;;    desktop name won't be saved.
  ;;  Saves files like '.emacs.desktopN' instead of 'name.emacs.desktop', strangely.
  ;;  Always locks .emacs.desktop.lock regardless of desktop you load, so every desktop
  ;;    load will give you a lock warning.  [PATCHED by me to lock specific desktop.]
  ;;  Doesn't unlock desktop on exit.  desktop-kill will do this (in fact it is done
  ;;    automatically in kill-emacs-hook) but knows nothing about our multiple desktops;
  ;;    if desired, add desktop-menu-kill which would use desktop-menu--current-desktop to set
  ;;    desktop-dirname and desktop-base-{file,lock}-name and call desktop-kill.
  ;;  Will often leave several minibuffer windows open.
  ;;  L (list buffers) appends to *desktop buffer list* instead of clearing first.
  ;;    Workaround: use 'q' to exit desktop buffer list.
  )

(use-package yaml-mode :defer t)

(require 'init-dired)

(require 'init-scheme)
(require 'init-c)
(require 'init-perl)
(require 'init-ocaml)
(require 'init-rust)
(require 'init-haskell)
;(require 'init-tex)

;;;; artist

(use-package artist
  :bind (:map artist-mode-map
              ;; Can't get rebind of s-mouse-1 to mouse-2 to work with artist mode, so manually
              ;; rebind the context menu from mouse-2 to s-mouse-1.
              ;; Cmd-click for middle click.
              ("<s-mouse-1>" . artist-mouse-choose-operation)))

;;;; bookmarks
(setq bookmark-default-file (locate-user-emacs-file "bookmarks"))
(setq bookmark-save-flag 1)

;;;; tramp

(use-package tramp
  :defer 2
  :init
  ;; Bugfix for Tramp 2.5.0.2, evidently this function is not autoloaded from tramp-crypt.
  ;; Instead of loading it or pinning an earlier version, define as a noop since
  ;; we don't use it.
  (defun tramp-register-crypt-file-name-handler ())
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(require 'init-aquamacs)
