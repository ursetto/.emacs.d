;;					 -*- outline-cycle-min-level: 2; -*-
;; $Id: .emacs,v 1.47 2014/11/26 17:37:31 jim Exp jim $

;; M-x locate-library RET org RET : Get filesystem location of package "org"
;; M-x list-load-path-shadows : List Emacs Lisp files that shadow other files
;; `kill -USR2 emacs-pid`  if Emacs hangs; then use `M-x toggle-debug-on-quit` afterward.

;; Invoke early-init in versions that don't do so automatically.
(when (version< emacs-version "27")
  (load (locate-user-emacs-file "early-init.el")))

(setq inhibit-startup-screen t)

;; Changing gc size and nulling handler list saves about 150 ms in startup costs.
(defvar gc-cons-threshold--orig gc-cons-threshold)
(defvar file-name-handler-alist--orig file-name-handler-alist)
(setq gc-cons-threshold 50000000
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook      ; reset these after we are done
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold--orig
                  file-name-handler-alist file-name-handler-alist--orig)))

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
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;;(global-set-key (kbd "C-x g") 'magit-status)   ;; was: C-c m
;;(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)  ;; was: C-c C-m
;; magit-status-mode-map: C-<tab> and M-<tab> are unreachable (section 4.2.2 Section Visibility). One of these should be set to C-c <tab>, at least. S-<tab> is reachable.

(use-package indent-tools
  :bind (("C-c >" . indent-tools-hydra/body))
  :config (use-package s)  ;; Required for up/down motion; not autoloaded
  )
(use-package undo-tree
  ;; C-x u to visualize; p/n/b/f to navigate; d to diff; t for timestamps; C-q to abort; M-_ to redo
  ;; Also see ~/.emacs.d/elpa/undo-tree-readme.txt
  :diminish
  :init (global-undo-tree-mode))

;;;; session -- save command and variable history across sessions
;; session can't be downloaded from MELPA because it has old, buggy version 2.3a.
;; I've submitted a PR for version 2.4b (https://github.com/emacsorphanage/session/pull/2)
;; but it may be wise to migrate to a maintained package. Meantime, use local copy in lisp/.
;; Note: session-initialize writes a custom var which requires feature 'session.
(use-package session
  :ensure nil   ; don't grab from melpa
  :init (add-hook 'after-init-hook 'session-initialize))

(use-package edit-indirect
  ;; C-c C-c : commit     C-c C-k : abort     C-x C-s : update and continue
  ;; To find existing buffer, select entire region and edit again. Consider implementing
  ;; ability to autoselect region for editing when inside existing one.
  :bind (("C-x n e" . edit-indirect-region)))

(use-package which-key    ;; Display popup key bindings. Globally useful.
  :diminish
  :config
  (which-key-mode t))

;;;; Possible future requires
;; hideshow-mode for lisp?
;; http://www.gentei.org/~yuuji/software/windows.el
;; http://www.gentei.org/~yuuji/software/revive.el

;;; Initialization
;;;; Main

;; Set the font / window size here by setting initial-frame-alist.  Font name and
;; height should not be set in (custom-set-variables '(default (...))) or weird glitches will occur.
;(setq initial-frame-alist `((width . 118) (height . 71) (font . "fontset-fixedr12")))
;(setq initial-frame-alist `((width . 118) (height . 66) (font . "fontset-monaco")))
;(setq initial-frame-alist `((width . 118) (height . 77) (font . "fontset-fixedr11")))

(menu-bar-mode 0)
(setq-default line-spacing 1)   ;; Required for fixeer11; font height is slightly too small
;; Height needs to be increased for line-spacing 1, even though there only 77 lines.  I guess
;; height is calculated as if line-spacing were 0.  This has a deleterious effect on
;; certain functions which position based on screen height, e.g. slowsplit.el; however
;; this would also fail with differing line-heights anyway.

;(setq initial-frame-alist `((width . 118) (height . 86) (font . "fontset-fixeer11o")))
   ; (internal-border-width . 2) (border-color . "#4e3831")))
   ; Can't get border to work on Aquamacs -- present but always black -- using fringe as a replacement instead.
;(setq default-frame-alist initial-frame-alist)  ;; Propagate font/window size to all frames.

(set-language-environment "UTF-8")
   ;; Note: for unknown reason, we cannot change the set-input-method (for example, to TeX)
   ;; when MacOSX is in U.S. mode.  It works in U.S. Extended.  This only seems to be an
   ;; issue in Carbon Emacs, not Aquamacs.
(global-font-lock-mode 1)
(icomplete-mode 1)              ;; Completion of non-ido things like C-h v, C-h f
;; (iswitchb-mode t)            ;; Switch between buffers using substrings (using ido-mode instead)
;; Ignore these extensions during filename completion (works with ido and others).
(add-to-list 'completion-ignored-extensions "~/")
(add-to-list 'completion-ignored-extensions ".retry")
(add-to-list 'completion-ignored-extensions "__pycache__/")
(require 'scroll-bar)
(scroll-bar-mode nil)
(winner-mode t)                         ;; use C-c left/right to undo/redo window config
(when (eq window-system 'mac)   ;; Only in OS X GUI mode.  Not a great solution.
  (server-start)                          ;; for emacsclient
  )
(when (eq window-system 'ns)
  ;; Bind Command-click to middle mouse button. This is for artist mode, and doesn't work (menu is
  ;; bound on down-mouse-2 and can't get working).
  ;; (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
  )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(show-paren-mode t)
(setq show-paren-delay 0)
;(defadvice show-paren-function    ;; from http://www.emacswiki.org/emacs/ShowParenMode
;      (after show-matching-paren-offscreen activate)
;      "If the matching paren is offscreen, show the matching line in the
;        echo area. Has no effect if the character before point is not of
;        the syntax class ')'."
;      (interactive)
;      (if (not (minibuffer-prompt))
;          (let ((matching-text nil))
;            ;; Only call `blink-matching-open' if the character before point
;            ;; is a close parentheses type character. Otherwise, there's not
;            ;; really any point, and `blink-matching-open' would just echo
;            ;; "Mismatched parentheses", which gets really annoying.
;            (if (char-equal (char-syntax (char-before (point))) ?\))
;                (setq matching-text (blink-matching-open)))
;            (if (not (null matching-text))
;                (message matching-text)))))

					; M-x glasses-mode (readable CamelText)
(setq kill-whole-line 1)   ; C-k at beginning of line kills entire line
; (setq show-trailing-whitespace t)     ;; buffer-local; actually set via Customize
(setq backup-directory-alist (cons (cons "." ".~")  ;; All backup files saved to .~/
                                   backup-directory-alist))
(setq require-final-newline t) ;; Add final newline when not present (set to 'query to ask first)
(setq-default indent-tabs-mode nil)    ;; screw it, tab characters are dumb.

; (setq c-macro-preprocessor "/usr/bin/cpp -C")  ;; Does not work.  See ~/doc/OSX.txt.
(setq enable-recursive-minibuffers t)
;;;; Show line/column like (30,2)
(line-number-mode 1)
(column-number-mode 1)


;;;; Keybindings
(global-set-key [kp-enter] 'overwrite-mode)  ;; As good a key as any for now.
(global-set-key [?\C-\)] 'blink-matching-open)  ;; See 55.4.6 for explanation of binding C-)
                                                ;; Does not appear to work; matching parens are highlighted anyway
(global-set-key [?\C-c?\;] 'comment-region)     ;; C-c ; comments, C-u C-c ; uncomments,
                                                ;; numeric prefix adds that many ;s (or *s for C)
(global-set-key "\M-`" 'other-frame)         ;; Was overridden in CVS by tmm.el at some point; may no longer need
(global-set-key "\C-x\C-a" 'auto-fill-mode)  ;; Another option is refill-mode.
(global-set-key "\C-xk" 'kill-this-buffer)   ;; Don't ask for which buffer to kill every time
(global-set-key "\C-cz" 'zap-up-to-char)     ;; requires 'misc
(global-set-key "\C-x2" 'split-window-quietly-zb)   ;; see below
(global-set-key "\C-c\C-j" 'imenu)           ;; jump to definition in this file (python binding; also used for org-goto)

(require 'init-window)
(require 'init-scheme)
;;(require 'init-fontset)         ;; legacy, unused

;;;; Aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'ts 'tags-search)
(defalias 'ta 'tags-apropos)
(defalias 'ws 'whitespace-mode)
(defalias 'wst 'whitespace-toggle-options)
(defalias 'tail-mode 'auto-revert-tail-mode)

;; M-r -- (move-to-window-line): M-r center, M-0 M-r top, M-- M-r bottom

;;;; Terminal

(unless window-system
 ;(xterm-mouse-mode 1)
 (defun track-mouse (arg1))  ;; No idea, but complains it is void otherwise.
)

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

;; "Copy rectangle" from http://www.emacswiki.org/emacs/RectangleCommands
(defun my-copy-rectangle (start end)
  "Copy the region-rectangle instead of `kill-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
(global-set-key (kbd "C-x r M-w") 'my-copy-rectangle)

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
(global-set-key (kbd "C-x C-d") 'delete-file-and-buffer)   ;; override list-directory

;;; Modes
;;;; text-mode

;; Indent-rigidly doesn't insert space in blank lines.
;; This is really annoying.
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

;; Globally set this (even in other modes).
(global-set-key "\C-x	" 'indent-rigidly-even-when-blank)

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

;;;; Mark mode
(transient-mark-mode 1)
(setq-default mark-even-if-inactive nil)  ; nil: Region commands don't operate on disappeared highlighting

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

;; FIXME: should be autoloaded, or loaded after startup, just so package-initialize can work
;;        when called at the end of startup. (Although we call package-initialize earlier.)
;; NOTE: This adds 50 ms to startup, and ido-ubiquitous-mode adds another 30 ms.
(ido-mode t)                               ;; supersedes iswitchb-mode
;; (setq ido-max-prospects 12)             ;; max # of matching items
;; (setq ido-show-dot-for-dired t)         ;; Interferes with last directory RET traversal --
;;                                         ;; C-j or C-d is better option to get into dired.
;; (setq ido-enable-dot-prefix t)          ;; Initial . forces prefix match. If off, can match exts
(setq ido-enable-tramp-completion t)
;;(ido-ubiquitous-mode t)                    ;; Use ido everywhere (external library)
;; ido will obey completion-ignored-extensions (when ido-ignore-extensions is t, the default).

;;;; smex

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)   ; plain vanilla M-x

;; While smex is active in the minibuffer:
;;  `C-h f` runs describe-function on the currently selected command.
;;  `M-.` jumps to the definition of the selected command.
;;  `C-h w` shows the key bindings for the selected command.

(require 'init-haskell)

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

;;;; AUCTex

;; With Skim installed, you can jump in Skim to the currently selected
;; line in Aquamacs by using Command-Shift-click or C-c C-c J RET.
;; You must first Customize Group AuCTeX:Tex Commands and change 'latex'
;; to 'latex -synctex=1'.
;; Also, you can reverse search (Skim->Aquamacs) with Command-Shift-Click
;; after you set Skim Preferences->Sync to know about Aquamacs [make sure
;; the path to emacsclient is correct; it may not be in your PATH].
(defun cons* (a b &rest rest)
  (cons a (if (null rest) b
	    (apply #'cons* b rest))))

(add-hook 'TeX-mode-hook
          (lambda ()
	    (outline-minor-mode t)
	    (define-key TeX-mode-map "\C-ct" 'TeX-command-master-LaTeX)
	    (define-key TeX-mode-map "\C-ce" 'LaTeX-emph-word)
	    (add-to-list 'TeX-output-view-style
			 ;; Use Skim.app for pdf output; though Jump to PDF uses Skim,
			 ;; regular View does not
			 '("^pdf$" "." "open -a Skim %o"))
	    ))
(setq TeX-parse-self t)

;; Run LaTeX without confirming it;
(defun TeX-command-master-LaTeX ()
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file nil)
  ;  (TeX-view)
)

(defun LaTeX-command-word (word)
  (interactive)
  (save-excursion
    (forward-word)
    (insert "}")
    (backward-word)
    (insert (concat "\\" word "{"))))

(defun LaTeX-command-region (word)
  (interactive)
  (let ((region (and transient-mark-mode mark-active)))
    (if region
        (save-excursion
	  (goto-char (region-end))
	  (insert "}")
	  (goto-char (region-beginning))
	  (insert (concat "\\" word "{")))
      (LaTeX-command-word word))))

(defun LaTeX-emph-word () 
  (interactive)
  (LaTeX-command-word "emph"))


;;;; ccmode

;; Except for c-offset-alist, the alists completely override any inherited
;; values from their parents.  E.g., c-hanging-braces-alist must be fully
;; specified.  For that reason, it's easier to just specify everything
;; completely rather than inheriting.

;; cc-mode uses c-beginning-of-defun when scanning internally for beginning of C
;; function.  This contrasts with the doc which claims beginning-of-defun [see
;; ccmode.info, 14 Performance Issues].  Therefore a { in column 0 should not
;; improve syntax parsing speed.  We do bind C-M-a to beginning-of-defun
;; to get the nicer defun-open search behavior.

;; font-lock-beginning-of-syntax-function, in ccmode, is set to c-beginning-of-syntax
;; as opposed to beginning-of-defun (19.3 Font Lock mode).  So left margin brace
;; should not improve scan speed either.

(defconst jim-c-style
  '(  ;; taken mostly from "linux" style with some KOS influence
    (c-basic-offset         . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-entry-open)
			       (brace-list-close)
			       (brace-entry-close)
			       (brace-list-intro)
			       (substatement-open after)
			       (block-close . c-snug-do-while)
			       (defun-open after) ; May slow down font lock if { is not in column 0.
			       (class-open after) 
			       (class-close)))
    (c-cleanup-list         . (brace-else-brace))
    (c-offsets-alist        . ((statement-block-intro . +)
			       (knr-argdecl-intro . 0)
			       (substatement-open . 0)
			       (substatement-label . 0)
			       (label . 0)
			       (statement-cont . +)
			       (case-label . *)
			       (statement-case-intro . *)
			       (arglist-cont-nonempty ;c-lineup-string-cont
						      c-lineup-argcont   ; subsumes string-cont
						      c-lineup-arglist)
			       (arglist-cont ;c-lineup-string-cont 
					     c-lineup-argcont
					     c-lineup-arglist)  )))
  "Jim's C style")

(add-hook 'c-mode-hook 
  (lambda () 
    (setq indent-tabs-mode nil)
    (define-key c-mode-base-map "\C-m" 'c-context-line-break) ;; newline-and-indent
    (define-key c-mode-base-map "\C-\M-a" 'c-beginning-of-defun)
    (define-key c-mode-base-map "\C-\M-e" 'c-end-of-defun)
    (c-toggle-auto-hungry-state 1)  ;; turn auto-hungry (C-c C-t) on
                                    ;; == auto-newline (C-c C-a) + hungry-delete (C-c C-d)
    (c-set-style "jim")             ;; set my style for the current buffer
                                    ;; Also possible to use (c-add-style "jim" jim-c-style t)
))

(add-hook 'c++-mode-hook       ;; Currently equivalent to c-mode-hook; could use c-mode-common-hook instead
  (lambda () 
    (setq indent-tabs-mode nil)
    (define-key c-mode-base-map "\C-m" 'c-context-line-break) ;; newline-and-indent
    (define-key c-mode-base-map "\C-\M-a" 'c-beginning-of-defun)
    (define-key c-mode-base-map "\C-\M-e" 'c-end-of-defun)
    (c-toggle-auto-hungry-state 1)  ;; turn auto-hungry (C-c C-t) on
                                    ;; == auto-newline (C-c C-a) + hungry-delete (C-c C-d)
    (c-set-style "jim")             ;; set my style for the current buffer
                                    ;; Also possible to use (c-add-style "jim" jim-c-style t)
))

(eval-after-load "cc-mode"
  '(c-add-style "jim" jim-c-style nil))

; (setq c-mode-hook nil) ;; For testing

;;;; cperl-mode

;; good values for cperl-array: sandybrown, tan, pink

;; Note: autoinsert is schizophrenic. { only is electric after
;; a () expression, so else { does not insert a newline.  You
;; must have cperl-electric-keywords enabled for this, which
;; will insert the whole construct.  sub { is never expanded.
;; Doubling braces/parens is silly, because you can't "type over"
;; the closing delimiter, but rather have to skip it with cursor
;; motion or use cperl-linefeed (C-c C-j).  The closing } in
;; an autoinserted construct must be skipped over manually.
;; After }, newline is always inserted and following 'else' is not
;; cleaned up, so you have to do this manually.  After ;, inserted
;; newline will not be deleted if you immediately type }, so you
;; have to delete it manually.
;; Tried 5.22 (newest), but compilation error, and when fixed, it
;; broke HEREDOC highlighting and had other weird font problems.
;; And it didn't fix any of these issues.
(add-hook 'cperl-mode-hook 
  (lambda () 
    (setq indent-tabs-mode nil)
    (setq cperl-indent-level 4)
    (setq cperl-highlight-variables-indiscriminately t)
    (setq cperl-electric-lbrace-space nil)
    (setq cperl-electric-parens nil)
    (setq cperl-auto-newline nil)
    (setq cperl-electric-keywords nil)
    (define-key cperl-mode-map "\C-m" 'newline-and-indent)
    (define-key cperl-mode-map "\C-j" 'cperl-linefeed)
))

;;;; VC

;; Set to nil to disable all VC mode backends and just use dedicated packages like magit for
;; version control. This may speed up tramp remote operations. This may have unexpected
;; consequences -- for example, project.el uses the VC backend to find the project root by
;; default. In particular, disabling Git can prevent lsp and elgot from guessing the project root,
;; so you can can reenable it in a mode hook for those.
(setq vc-handled-backends nil)

;;;; Gforth

;; forth-mode.el is gforth.el from the gforth distribution, not available as package.
;; Renamed to forth-mode.el so it provides 'forth-mode properly.
;; Recommendation: Try https://github.com/larsbrinkhoff/forth-mode from MELPA.
(use-package forth-mode
  :ensure nil  ; use local copy
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

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;(autoload 'csv-mode "csv-mode"
;  "Major mode for editing comma-separated value files." t)
(setq csv-header-lines 1)    ;; Assume all csv files have a header, for auto region select

;;;; html-mode

(defun html-tidy-buffer ()
  (interactive)
  (mark-whole-buffer)
  (shell-command-on-region (point-min) (point-max)
			   "tidy -w 0 -xml -q -i" 
			   t   ; other output buffer (n/a)
			   t   ; replace
  ))

(require 'init-dired)

;;;; woman / man-mode

;; Advisory only:
;; woman-mode sucks.  1) It is supposed to set buffer tab-width to woman-tab-width, but this
;; does not happen, so the width is totally screwed up.  (A better way would be to
;; expand tabs to spaces.)  2) Rereading manpath cache with `C-u M-x woman` does not work;
;; you have to quit emacs.  3) Sometimes it opens in the wrong frame.  4) It does not read
;; all manpages, for example OS X printf(3).

;; man mode is really slow, but formatting is correct.
;; newframe puts man page in new frame -- but new every time!  That's somewhat useless.
;; default tabs are 8 and untabify is called before man-mode-hook -- so we cannot set
;;   the tab-width.  Man-cooked-hook is called before, but Man-mode runs
;; (kill-all-local-variables) and undoes our changes to tab-width.

;; Speed can be increased a lot by not using -a switch.
;; 

;; (setq Man-switches "-a")  ;; Display all man page sections; use M-n / M-p to traverse

(add-hook 'Man-mode-hook
	  (lambda ()
	    (outline-minor-mode t)
;	    (set-frame-name "*Man*")  ; won't work
	    ))
(setq Man-frame-parameters
      '((width . 78) (height . 40)))

(setq Man-notify-method 'newframe)  ;; Overridden below in Aquamacs section.
;; Discussion. Ideally we might have a dedicated frame for man pages, including a
;; permanent buffer-selection buffer displaying just man page buffers.
;; Right now, I cannot send all man buffers to the same frame; special-display-regexps
;; will send differently-named buffers to different frames, even if the same regexp
;; matches.  If only I could specify a frame _name_ to send it to.

;; test
(defun man-display-buffer-in-man-frame (buffer)
  (select-frame-by-name "*Man*")
  (switch-to-buffer buffer)  ; this and switch-to-buffer-here are affected weirdly
                             ; seems to be a property of the special windows themselves
                             ; for example switching to a new buffer always makes a new window
                             ; may have to do with 'dedicated windows' (window-dedicated-p)

  
)

;;;; desktop

;; desktop-menu is unsupported and can only be found as a download from emacswiki.org.
;; Recommend looking for a supported package. See: https://www.emacswiki.org/emacs/SessionManagement
(use-package desktop-menu         ;; implicitly loads 'desktop
  :ensure nil   ; not a package
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

(require 'init-ocaml)
(require 'init-rust)

;;;; artist

(eval-after-load 'artist
  '(progn
     ;; Can't get rebind of s-mouse-1 to mouse-2 to work with artist mode, so manually
     ;; rebind the context menu from mouse-2 to s-mouse-1.
     (define-key artist-mode-map (kbd "<s-mouse-1>") 'artist-mouse-choose-operation)    ; Cmd-click for middle click
     ))

;;;; bookmarks
(setq bookmark-default-file (locate-user-emacs-file "bookmarks"))
(setq bookmark-save-flag 1)
;;;; tramp

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; Random crap
;; insert test1 .. test5 into buffer
;; (mapcar (lambda (x) (insert "test" (format "%d" x) "\n")) '(1 2 3 4 5))
;; (let ((line 1)) (while (<= line 5) (insert "test" (format "%d" line) "\n") (setq line (+ line 1))))

;; repeat last command:             C-x z   (z to repeat further),   M-x repeat
;; repeat minibuffer command:       C-x M-:  or C-x ESC ESC  or M-x repeat-complex-command (like M-x M-p, but keeps arguments, as a lisp expression)
;; show minibuffer command history: M-x command-history
;; show last 100 chars typed:       M-x view-lossage, C-h l

(require 'init-aquamacs)
