;;					 -*- outline-cycle-min-level: 2; -*-
;; $Id: .emacs,v 1.47 2014/11/26 17:37:31 jim Exp jim $

;; M-x locate-library RET org RET : Get filesystem location of package "org"
;; M-x list-load-path-shadows : List Emacs Lisp files that shadow other files
;; `kill -USR2 emacs-pid`  if Emacs hangs; then use `M-x toggle-debug-on-quit` afterward.

;;; Requires

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; [This is done in my/package.el for now.]
; (package-initialize)

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

(add-to-list 'load-path "~/.emacs.d/lisp")
(autoload 'lisppaste "lisppaste"      ;; Use M-x lisppaste to start
 "Major mode for interacting with the lisppaste bot." t)
(autoload 'filladapt-mode "filladapt" ;; M-x filladapt-mode to enable minor mode
 "Minor mode for smart guesses on fill-prefix and paragraph boundaries." t)
(autoload 'setnu-mode "setnu"
 "Display line numbers as in :set nu." t)
(autoload 'zap-up-to-char "misc" "Load this for zap-up-to-char" t)
(autoload 'sepia-init "sepia" "Emacs-Perl interaction REPL." t) ;; Use M-x sepia-init to start
;(defalias 'perl-mode 'cperl-mode)

;; Configure and load ELPA (M-x list-packages).  Uses the global install.
(load-file (locate-user-emacs-file "my/package.el"))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

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
  :init (global-undo-tree-mode))

;;;; session
(require 'session)             ;; save command and variable history across sessions
(add-to-list 'session-globals-exclude 'org-mark-ring)  ;; avoid infinite loop on exit
(add-hook 'after-init-hook 'session-initialize)

(use-package edit-indirect
  ;; C-c C-c : commit     C-c C-k : abort     C-x C-s : update and continue
  ;; To find existing buffer, select entire region and edit again. Consider implementing
  ;; ability to autoselect region for editing when inside existing one.
  :bind (("C-x n e" . edit-indirect-region)))

;;;; Possible future requires
;; hideshow-mode for lisp?
;; http://www.gentei.org/~yuuji/software/windows.el
;; http://www.gentei.org/~yuuji/software/revive.el
;; (require 'breadcrumb)        ;; future -- cycle through bookmarks -- see breadcrumb.el (@ breadcrumb.sf.net)

;;--This was in here before, but I don't appear to use it.
;; (require 'filesets)
;; (filesets-init)                ;; M-x filesets-open, M-x filesets-edit

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
(icomplete-mode 1)
;; (iswitchb-mode t)            ;; Switch between buffers using substrings (see also icicle.el, ido-mode)
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
(global-set-key "\C-x\C-b" 'bs-show)
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

(load (locate-user-emacs-file "my/my-window.el"))  ;; TODO: replace with require

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

;;; Fontsets

(when (eq window-system 'w32)
  (set-face-font 'default "Lucida Sans Typewriter-8.0:antialias=none")
)

(when (eq window-system 'mac)

;; Use M-x set-frame-font to temporarily set font of current frame (hit TAB for completion)

(setq mac-allow-anti-aliasing t)

;; There are still some Carbon font glitches where Carbon Emacs doesn't look as good as the Cocoa Terminal.
;; You can especially see this on accented chars: éíóö.

;; iso10646-1 is the correct font encoding to use for Unicode, but Monaco iso10646-1 was formerly
;; displayed anti-aliased even with anti-aliasing off.  Finally, on 2008-12-11 a fix was
;; discovered at http://homepage.mac.com/nand/macosx/antialiasing.html :
;; $ defaults write org.gnu.Emacs AppleAntiAliasingThreshold 14
;; $ defaults write org.gnu.AquamacsEmacs AppleAntiAliasingThreshold 14
;; Monaco:ascii with iso10646-1 doesn't space correctly, though.  Everything is too thin.  Instead,
;; use the mac-roman encoding for the ascii space.  For fixedr, bold spacing still isn't fixed.

;; Use iso10646-1 instead of mac-roman for the latin-iso8859-1 space in order to support
;; all iso-8859-1 characters correctly -- try ×, the U+00D7 times character, which looks
;; like 'W' under mac-roman.  However, it will be antialiased under Carbon unless you set
;; the AppleAntiAliasingThreshold as above.

;; Using Aquamacs seems to fix some font issues, but -- so far -- only when you select a
;; single font (such as Fixee) instead of a fontset.  For example, letters such as λ do
;; not cause extra vertical line spacing.  Some characters claim to come from
;; Fixee upon C-u C-x =, such as λ, but are clearly from Monaco (increase the size to see
;; that).  Also, Fixee as selected from the menu seems to use some
;; proportional (?) characters such as π and “.
;; fixedr12 no longer introduces extra vertical space.  However, my fixedr11 fontset has
;; more vertical space than selecting "Fixee 11" from the Font menu; the latter is the equivalent
;; of selecting the smallest possible vertical space in Terminal--which is too small.
;; Given that, it seems like it's possible to tweak the line spacing somehow, unless
;; Quartz is handling it internally.
;; Indeed, describe-font for menu Fixee shows
;;   name (opened by): -apple-fixee-medium-r-normal--11-0-72-72-m-0-iso10646-1
;;   full name: -apple-fixee-medium-r-normal--11-110-72-72-m-110-iso10646-1
;;   height:10
;; while for fontset it shows
;;   name (opened by): -apple-fixed-medium-r-normal--11-110-72-72-m-110-mac-roman
;;   full name: -apple-fixed-medium-r-normal--11-110-72-72-m-110-mac-roman 
;; Notice that it is called "fixee" and not "fixed" and that it is opened without
;; a 110.

;; Monaco 10
(create-fontset-from-fontset-spec "-apple-monaco-medium-r-normal--10-*-*-*-*-*-fontset-monaco,ascii:-apple-monaco-medium-r-normal--10-100-*-*-m-100-mac-roman,latin-iso8859-1:-apple-monaco-medium-r-normal--10-100-*-*-m-100-iso10646-1")

;; fixedr11: from fixed-6x11-reshape.bdf
(create-fontset-from-fontset-spec 
 (concat "-apple-fixed-medium-r-normal--11-*-*-*-*-*-fontset-fixedr11,"
         "ascii:-apple-fixed-medium-r-normal--11-*-*-*-m-*-mac-roman,"
         ;; Use monaco for non-ascii.  We don't have an iso10646-1 encoding for fixed.
         ;; Note: 10pt monaco height is too high for fixed11.
         "latin-iso8859-1:-apple-monaco-medium-r-normal--10-*-*-*-m-*-iso10646-1,"
         "mule-unicode-0100-24ff:-apple-monaco-medium-r-normal--10-100-*-*-m-100-iso10646-1,"
         ;; greek required.  Quack composed lambda falls under this, not mule-unicode, and
         ;; will default to '-apple-abadi mt-12'.  Don't know how to change default.
         ;; (It doesn't work to set the font pattern to monaco.)
         "greek-iso8859-7:-apple-monaco-medium-r-normal--10-*-*-*-m-*-iso10646-1,"
         ))

;; fixedr12: from fixed-6x12-reshape.bdf.  Same as fixedr, but
;; with an extra blank vertical line for spacing.
(create-fontset-from-fontset-spec 
 (concat "-apple-fixed-medium-r-normal--12-*-*-*-*-*-fontset-fixedr12,"
         "ascii:-apple-fixed-medium-r-normal--12-*-*-*-m-*-mac-roman,"
         "latin-iso8859-1:-apple-monaco-medium-r-normal--10-*-*-*-m-*-iso10646-1,"
         "mule-unicode-0100-24ff:-apple-monaco-medium-r-normal--10-100-*-*-m-100-iso10646-1,"
         "greek-iso8859-7:-apple-monaco-medium-r-normal--10-*-*-*-m-*-iso10646-1,"
         ))

;; Seems to fix most font issues with both Aquamacs and Carbon Emacs.  Line
;; spacing is uniform (I guess Quartz must be picking substitute fonts
;; automatically) even with Unicode chars.  Only problem is certain proportional
;; Unicode characters such as π and “ are used; I prefer the Monaco versions,
;; but using Monaco will increase line spacing.  That I don't know how to fix.
;; I think I fixed the TrueType crashing problem by leaving the '0' spacing
;; rather than using 110 as before.

;; NOTE!!: Aquamacs 1.6 uses per-mode styling, set via Options->Appearance.
;;         (See ~/Library/Preferences/Aquamacs\ Emacs/customizations.el).
;;         I selected "Fixee 11pt" from the Font menu, and it seems to have
;;         automatically selected fixeer11o; possibly because it's the
;;         only Fixee fontset specified.
;;         
;;         Also, we must set japanese-jisx0208, as it otherwise defaults to the
;;         kanjistrokeorders font I installed earlier, which is unreadable.
;;         Setting it here appears to tell Emacs to use it everywhere even when
;;         per-mode fonts are not set to Fixee.
(create-fontset-from-fontset-spec 
 (concat "-apple-fixee-medium-r-normal--11-*-*-*-*-*-fontset-fixeer11o,"
         "ascii:-apple-fixee-medium-r-normal--11-0-72-72-m-0-iso10646-1,"
         "latin-iso8859-1:-apple-fixee-medium-r-normal--11-0-72-72-m-0-iso10646-1,"
         "mule-unicode-0100-24ff:-apple-fixee-medium-r-normal--11-0-72-72-m-0-iso10646-1,"
         "greek-iso8859-7:-apple-fixee-medium-r-normal--11-0-72-72-m-0-iso10646-1,"
;;       "japanese-jisx0208:-apple-hiragino kaku gothic pro-medium-r-normal--18-*-*-*-m-*-iso10646-1,"
         "japanese-jisx0208:-apple-ms pmincho-medium-r-normal--16-0-72-72-m-0-iso10646-1,"
;;         "japanese-jisx0208:-apple-ms pgothic-medium-r-normal--16-0-72-72-m-0-iso10646-1,"
         ))

;; fontset-mincho24

;(create-fontset-from-fontset-spec "-apple-fixed-medium-r-normal--12-*-*-*-*-*-fontset-fixedr12,ascii:-apple-fixed-medium-r-normal--12-120-*-*-m-120-mac-roman,latin-iso8859-1:-apple-monaco-medium-r-normal--10-100-*-*-m-100-mac-roman,mule-unicode-0100-24ff:-apple-monaco-medium-r-normal--10-100-*-*-m-100-iso10646-1")


;; (set-fontset-font "fontset-fixedr12" 
;;                    (make-char 'mule-unicode-0100-24ff #x27 #x3B)
;;                    "-apple-monaco-medium-r-normal--10-100-*-*-m-100-iso10646-1")
;;(set-fontset-font "fontset-fixedr12" 
;;                  (make-char 'greek-iso8859-7 107) 
;;                  "-apple-monaco-medium-r-normal--10-100-*-*-m-100-iso10646-1")


;;;; Mincho

;; NB: The jisx0208:-apple-ms mincho-... was originally in kanji but for whatever
;; reason, this causes a bunch of shift-jis etc. stuff to be brought in on
;; startup -- just by reading this file!  Even when commented out!  So, I have
;; changed it to ascii text.  Untested, and may be obsolete anyway.
;; (create-fontset-from-fontset-spec 
;;  (concat "-apple-ms mincho-medium-r-normal--24-*-*-*-*-*-fontset-mincho24,"
;;          "ascii:-apple-ms mincho-medium-r-normal--24-*-*-*-m-*-iso10646-1,"
;;          "japanese-jisx0208:-apple-ms mincho-medium-r-normal--24-*-*-*-m-*-jisx0208.1983-sjis"))

;;;; Hiragino fontsets

(create-fontset-from-fontset-spec 
 (concat "-apple-andale mono-medium-r-normal--18-*-*-*-*-*-fontset-hiragino18a,"
         "ascii:-apple-andale mono-medium-r-normal--18-*-*-*-m-*-iso10646-1,"
         "japanese-jisx0208:-apple-hiragino kaku gothic pro-medium-r-normal--18-*-*-*-m-*-iso10646-1"))

;; Carbon Emacs' font rendering is bad; hiragino kaku is fine at 16pt
;; under Cocoa, but must be 24 pt to be legible under Carbon.
(create-fontset-from-fontset-spec 
 (concat "-apple-hiragino kaku gothic pro-medium-r-normal--18-*-*-*-*-*-fontset-hiragino1824,"
         "ascii:-apple-hiragino kaku gothic pro-medium-r-normal--18-*-*-*-*-*-iso10646-1,"
         "japanese-jisx0208:-apple-hiragino kaku gothic pro-medium-r-normal--24-*-*-*-*-*-iso10646-1"))

(defun mincho-frame ()
  "Set up selected frame for Mincho 24."
  (interactive)
  (set-foreground-color "black")
  (set-background-color "white")
  (set-frame-font "fontset-mincho24")
  (set-frame-width  (selected-frame) 104)
  (set-frame-height (selected-frame) 33))

(defun monaco24-frame ()
  "Set up selected frame for Monaco 24."
  (interactive)
  (set-foreground-color "black")
  (set-background-color "white")
  (set-frame-font "fontset-monaco24")
  (set-frame-width  (selected-frame) 85)
  (set-frame-height (selected-frame) 25))

(defun hiragino1824-frame ()
  "Set up selected frame for Hiragino 18+24."
  (interactive)
  (set-foreground-color "white")
  (set-background-color "black")
  (set-frame-font "fontset-hiragino1824")
  (set-frame-width  (selected-frame) 130)
  (set-frame-height (selected-frame) 31))

(defun heisig ()
  "Load Heisig Kanji Data in an appropriately configured new frame."
  (interactive)
  (let ((f (make-frame)))
    (select-frame f)
    (hiragino1824-frame)
    (find-file-existing (expand-file-name "~/prog/python/heisig/data/Heisig Kanji Data.utf8"))))

)  ; end (eq window-system 'mac)

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

;;;; outline-mode
;;;;; Comments
;; TAB (Overview) does not work for ELisp files; the whole file is collapsed into ...
;; That's because OVERVIEW does (hide-sublevels 1) [C-c @ C-q] and the ;;; lines
;; are at minimum level 3.  Modified outline-magic.el to take a minimum-level
;; variable.

;; To show more than 1 header in an overview, use C-u # M-x hide-sublevels.
;; 9 may be a reasonable number to show quite a bit [5 is minimum for default regexp].
;; Note that you can modify outline-cycle-minimum-header-level to do this automatically
;; for OVERVIEW.

;; I have now added a lisp-outline-level override [superseding lisp-mode.el] which
;; counts the semicolons and subtracts 2.  Therefore, ;;; counts as level 1.

;; Setting outline-minor-mode: 1 in local variables list fails.  The hook does not appear
;; to be called correctly -- either cycling doesn't work, or the ... suffix is not displayed.
;; I am forced to put it in the mode hook.

;;;;;; Outline styles
;; (setq outline-regexp ";;;+ [^ 	\n]\\|(")
;;    In this style ;;; are top-level headers and ;;;; ;;;;; etc. are subheaders.
;;    Toplevel ( are folded [in conjunction with scheme-outline-level below, level is
;;    set to 1000, guaranteeing toplevel forms will be expanded last.]
;;    No one uses this style except me, apparently.
;; (setq outline-regexp ";;; [^ 	\n]\\|(")
;;    ;;; are top-level headers; there are no subheaders.  This helps when people
;;    use ;;;; as the file header and ;;; as regular headers, which is common
;;    in the Chicken world.
;; Alternatively, we can still match ;;;+ and have scheme-outline-level return
;; level 1 for any number of semis, so ;;; ;;;; ;;;;; etc. are all considered
;; toplevel headers.  Not perfect, but better for Chicken use.
;; 
;; (setq outline-regexp ";;;;+ [^ 	\n]\\|(")
;;    Follows Riastradh's style guide wherein ;;;; is a toplevel header and ;;;;;+
;;    are subheaders, and ;;; are merely toplevel comments.
;;    May not work correctly with existing lisp-outline-level, which assigns level 1 to ;;;.
;;    Appears to be uncommon--people who use ;;; for toplevel comments (c.f. htmlprag)
;;    may use ;;; for headers, or skip headers entirely.
;; None of these address the no-header issue -- if the first line is not a header,
;; outline-mode collapses the entire document and you cannot cycle, only expand it
;; completely with (show-all).  The current solution is simply to not collapse
;; the document automatically on load, and rely on the user to not collapse
;; headerless documents manually.


;;;;; Customization
(add-hook 'emacs-lisp-mode-hook
          (function (lambda ()
                      (outline-minor-mode t)
		      ;; Default regexp includes min ;;; plus space plus first char, totalling 5 chars
 		      (setq outline-level 'lisp-outline-level)   ; the default
                      (when (fboundp 'outline-cycle)        ; only if outline-magic is loaded
                        (outline-cycle nil) ;; OVERVIEW mode, but won't respect local min-level yet
                        (setq outline-cycle-min-level 1))
					; (hide-other)
                      )))
(eval-after-load 'emacs-lisp-mode
  '(progn (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)))

;; This hook is only for outline-minor-mode related stuff!
(add-hook 'scheme-mode-hook
          (function (lambda ()
                      (outline-minor-mode t)
		      (setq outline-regexp ";;;+ [^ 	\n]\\|(")   ;; similar to emacs-lisp-mode
 		      (setq outline-cycle-min-level 1)
 		      (setq outline-level 'scheme-outline-level)
                      (idle-highlight-mode t)
		      ;; Enable scheme-complete on TAB and move outline cycling from
		      ;; TAB to M-TAB.  Warning, this is global, and should not be done.
;; 		      (define-key outline-minor-mode-map [(tab)] nil)
; 		      (outline-cycle nil) ;; OVERVIEW mode
; 		      (outline-cycle nil) ;; CONTENTS mode
                      )))

;; This is an unfortunate hack to restore the binding of little "tab", because
;; is hijacked everywhere by (require 'outline-magic).  org-mode only binds
;; big TAB (ctrl-i) and expects little "tab" (tab key) to reflect to big TAB.
;; Otherwise we can't create tables.  Note, orgtbl-mode still works without
;; this hack, because it intercepts (tab) directly.
(add-hook 'org-mode-hook
	(lambda ()
		(define-key org-mode-map [(tab)] 'org-cycle)
		;(setq org-allow-space-in-links nil)
        ))

;; - Enable outline cycling.
;; (add-hook 'outline-mode-hook 
;;   (lambda () (require 'outline-magic)))  ;; disabled due to conflicts with org mode cycling,
                                            ;; but org-mode can cycle OVERVIEW with S-Tab.


;; Set up outline-magic.  Comment out entire hook if not installed.  Note that missing outline-magic
;; may circularly prevent installing outline-magic from ELPA, probably due to outline use from elisp mode.
(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
	    ;;(define-key outline-minor-mode-map [(tab)] 'outline-cycle)
	    (define-key outline-minor-mode-map [(M-tab)] 'outline-cycle)
	    (define-key outline-minor-mode-map "\C-c\t" 'outline-cycle) ; meh
	    (setq outline-cycle-emulate-tab nil)
	    (define-key outline-minor-mode-map [(S-tab)]
	      '(lambda () (interactive) (outline-cycle '(4))))))  ;; magic '(4) is for OVERVIEW cycle

; Warning: in outline-minor-mode, tab emulation calls indent-relative,
; and will not honor any mode-specific indentation.  So I originally bound cycling
; to S-tab (unused in minor-mode).  Unfortunately this extends to
; org-mode and TAB will override the nice org-mode behavior with the
; simpler outline-magic behavior.  I tried changing indent-relative to
; indent-for-tab-command, but it did not work.
; Nevertheless: indent-for-tab-command fixes my gripe with TAB in outline-minor-mode,
; so TAB is now the default cycle key.
;; (setq outline-cycle-emulate-tab t)
(defun lisp-outline-level ()
  "Lisp mode `outline-level' function (modified so ;;; is level 1)."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (if (looking-at "(\\|;;;###autoload")
	1000
      (if (looking-at ";;;+")
	  (let (buffer-invisibility-spec)  ; idea from http://www.cs.unc.edu/~gb/Software.html
	    (save-excursion
	      (skip-chars-forward ";")
	      (- (current-column) 2) ;; count semis and subtract 2
	      ))   
	len))))

(defun scheme-outline-level ()
  "Scheme mode `outline-level' function (modified so ;;; is level 1)."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (if (looking-at "(\\|;;;###autoload")
	1000
      (if (looking-at ";;;+")
	  (let (buffer-invisibility-spec)  ; idea from http://www.cs.unc.edu/~gb/Software.html
	    (save-excursion
	      (skip-chars-forward ";")
	      1          ; any number of semis is considered level 1
	      ))   
	len))))


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


;;;; Info mode


;; Looks like this works without help now.  /usr/local/info is
;; included by default on Linux and OS X, and Aquamacs finds its own
;; info files even in nonstd location.

;; (eval-after-load 'info
;;   '(progn
;;      (add-to-list 'Info-default-directory-list "/usr/local/info")))


;;;; Org-mode
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
;(define-key global-map "\C-cr" 'org-remember)   ;; NOTE: Superseded by org-capture.
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cL" 'org-store-link) ;; Use L instead of l to avoid conflict with windmove
(global-set-key "\C-c\C-l" 'org-insert-link-global)   ;; Of dubious value
(global-set-key "\C-c\C-o" 'org-open-at-point-global) ;; Useful

(setq org-completion-use-ido t)         ;; depends: ido-mode

;; (add-hook 'org-mode-hook
;;   (lambda ()
;;     ;; note: used to have (org-todo 0) here, which skips note-taking when
;;     ;; transitioning to a note-taking state like WAIT.  Not sure why I did that.
;;     ;; Just use C-c C-c with empty note to get same behavior.  I think what I
;;     ;; *{meant} to do was have C-c t transition to the next state, which should
;;     ;; be like (org-todo 'right).
;;     (define-key org-mode-map "\C-ct"   ;; alias C-c C-t
;;       '(lambda () (interactive) (org-todo)))))

;; org-remember.
;(org-remember-insinuate)       ; superseded by org-capture
(setq org-directory "~/doc/")
(setq org-default-notes-file (concat org-directory "notes.txt"))
(setq org-remember-templates   ; use M-1 C-c C-c to store to arbitrary location
      '(("todo" ?t "* TODO %?\n  %i\n" "todo.txt" bottom)
	("to-read" ?r "* TODO %?\n  %i\n" "to-read.txt" bottom)
	("bills" ?b "* TODO %^{Bill name}\nDEADLINE: %^{Due date}t\n%?" "todo.txt" "Bills")
;        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/JOURNAL.org")
;        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/JOURNAL.org" "New Ideas"))
))
;; Alternatively use 'outline interactive mode instead of 'refile,
;; although I had some problems with stability.
(setq org-remember-interactive-interface 'refile)

;; org-capture
(setq org-capture-templates  ;; see [[info:org#Capture%20templates]]
 '(("u" "UAL")
   ("un" "UAL notes" entry (file+olp "~/work/ual/todo.txt" "Notes"))
   ("up" "UAL projects" entry (file+olp "~/work/ual/todo.txt" "Projects"))
   ("ui" "UAL incidents" entry (file+olp "~/work/ual/todo.txt" "Incidents")
    "* TODO IM%?\n%U\n")
   ("ut" "UAL tasks" entry (file+olp "~/work/ual/todo.txt" "Tasks")
    "* TODO %?\n%U\n") ;; :unnarrowed t)
))

;; org-attach: C-c C-a a  attach; C-c C-a o   open; C-c C-a f    show attachment dir
;; [[info:org#Attachments]]
(setq org-attach-method 'ln)   ;; hard link attachments
(setq org-outline-path-complete-in-steps nil)         ; nil if ido completion active
(setq org-goto-interface 'outline) ; or outline-path-completion -- C-u C-c C-j for alt interface
(setq org-refile-use-outline-path t)   ; Refile targets as lev1/lev2/lev3 paths.
(setq org-refile-targets '((nil . (:maxlevel . 2))))  ; Headings and files for refile targets; one idea is '(my-note-files . (:tag . "refile"))
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; See [[info:org#TODO%20extensions][info:org#TODO extensions]] for keyword ideas.  See [[info:org#Tracking%20TODO%20state%20changes][org info 5.3.2]] for TODO state tracking.
(setq org-todo-keywords '((type "TODO(t)" "WAIT(w)" "PEND(p)" "|" "DONE(v)" "SKIP(x)" "FAIL(f)")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "orange" :weight bold)
        ("DONE" :foreground "green")
        ("WAIT" :foreground "magenta" :weight bold)
        ("HOLD" :foreground "gray" :weight bold)
        ("PEND" :foreground "#cccccc")
        ("CNCL" :foreground "#666666")
        ("SKIP" :foreground "#666666")
        ("FAIL" :foreground "#666666")
        ))

(setq org-log-done t)  ;; Log CLOSED: timestamp when changing TODO to DONE
(setq org-log-into-drawer t)  ;; Log (@/!) timestamps and notes into LOGBOOK drawer (not including org-log-done)

(setq org-archive-location "%s_archive.txt::")  ; add .txt to archive files for Spotlight-ability

;; Don't update state change on S-cursor -- mainly here to avoid accidental timestamp
;; update when I hit shift-right, etc.  You could bind something to (org-todo 'right)
;; to override this setting.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-startup-indented t)
(setq org-use-speed-commands t)      ; press ? at headline bol for help
(setq org-return-follows-link t)
(setq org-cycle-include-plain-lists 'integrate)  ; integrate list display with tree cycling
(setq org-list-allow-alphabetical t)
(setq org-special-ctrl-a/e 'reversed)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) 
; note: this should go in (eval-after-load "org" (lambda () ...) if you want it
;(add-to-list 'org-modules 'org-habit)

;; agenda
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-start-on-weekday nil) ; start on today
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

(add-hook 'org-load-hook
          ;; Similar to "n" but show a longer view by default, skip scheduled items, and
          ;; skip empty dates. This has some aspects of the old 'timeline' view.
          ;; (We might just want to set some of these options as default.)
          (lambda ()
            (setq org-agenda-custom-commands    ;; not using add-to-list, may not be defined
                         '(("L" "Long view"
                           ((agenda ""
                                    ((org-agenda-span 90) ; 'month
                                     (org-agenda-show-all-dates nil)))
                            (alltodo ""
                                     ((org-agenda-todo-ignore-scheduled t)))))))))

(setq org-highest-priority ?A)
(setq org-default-priority ?C)
(setq org-lowest-priority ?E)

;;;; bs (buffer mode)
;; Keybinding bs-show is set in keybindings section.

(require 'bs)  ;; Although autoloaded via bs-show, we explicitly load this
               ;; so the variable bs-configurations is available.
;; Here we set the bs-must-show-regexp value of the "files" configuration to
;; always show the *scheme* buffer and any *info* buffers.
(setcar (cdr (assoc "files" bs-configurations))
	(concat "^\\*scheme\\*$"  "\\|"  ;; concat: elisp sec 4.3
		"^\\*info\\*"))

;; Stable sort when sorting by filenames.  Originally from
;; http://os.inf.tu-dresden.de/~mp26/download/swbuff-y.el
;; and cleaned up by me.  Customize bs-sort-functions to activate.
;; Note: Cloned file buffers don't have a filename and will not be grouped "correctly".
(defun bs--sort-by-filename-stable (b1 b2)
  "Compare buffers B1 and B2 by file name and as a secondary condition
   by buffer name."
  (if (string-equal (buffer-file-name b1) (buffer-file-name b2))
      (string< (buffer-name b1)
               (buffer-name b2))
    (string< (or (buffer-file-name b1) "")
             (or (buffer-file-name b2) ""))))

(defun bs--sort-by-filename-stable-empty-end (b1 b2)
  "Compare buffers B1 and B2 by file name and as a secondary condition
   by buffer name.  Empty filenames placed at end."
  (or (string< (buffer-file-name b1) (buffer-file-name b2))
      (and (not (string< (buffer-file-name b2) (buffer-file-name b1)))
           (string< (or (buffer-name b1) "")
                    (or (buffer-name b2) "")))))

;; Stable sort for sort-by-mode -- my authorship.
(defun bs--sort-by-mode-stable (b1 b2)
  "Compare buffers B1 and B2 by mode name.  Secondarily, 
   sort by filename."
  (save-excursion
    (let ((m1 (progn (set-buffer b1) (format "%s" mode-name)))
	  (m2 (progn (set-buffer b2) (format "%s" mode-name))))
      (if (string-equal m1 m2)
	  (bs--sort-by-filename-stable b1 b2)
          (string< m1 m2)))))

;; Like bs--get-file-name but abbreviates homedir to ~.
;; Customize Files in bs-attributes-list to activate.
(defun bs--get-file-name-abbrev (start-buffer all-buffers)
  "Return string for column 'File' in Buffer Selection Menu.
This is the variable `buffer-file-name' of current buffer.
If current mode is `dired-mode' or `shell-mode' it returns the
default directory.  Filenames are abbreviated (e.g. homedir -> ~).
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (propertize (if (member major-mode '(shell-mode dired-mode))
                  default-directory
                (if buffer-file-name (abbreviate-file-name buffer-file-name) ""))
              'mouse-face 'highlight
              'help-echo "mouse-2: select this buffer, mouse-3: select in other frame"))

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

;;;; smex

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)   ; plain vanilla M-x

;; While smex is active in the minibuffer:
;;  `C-h f` runs describe-function on the currently selected command.
;;  `M-.` jumps to the definition of the selected command.
;;  `C-h w` shows the key bindings for the selected command.

;;;; cscope

;; This takes some time at startup, so I'm requiring cc-mode be loaded
;; beforehand.  This isn't the right solution.  Perhaps better would be
;; load on "C-c s" prefix.  However, IIRC xscope does some weird
;; autoloading internally.
(eval-after-load "cc-mode"
 '(require 'xcscope))             ;; Cscope prefix: C-c s  (docs: ~/.emacs.d/xcscope.el)

;; xcscope.el doesn't automatically load on ObjC code.
;; If you don't want cscope mode to auto-load, you'll have to delete the hooks
;; from the end of xcscope.el, and start it manually with (cscope:hook).
(add-hook 'objc-mode-hook (function cscope:hook))

;;;; Scheme mode

(add-hook 'scheme-mode-hook
 (lambda ()
   (set (make-local-variable 'kill-whole-line) nil)
))

(defun chicken-doc ()
  (interactive)
  (let ((func (current-word)))
    (if func
	(process-send-string "*scheme*"
         (concat "(require-library chicken-doc) ,doc " func "\n")))))

(eval-after-load 'scheme
  '(progn
     (require 'quack)
     (define-key scheme-mode-map "\C-cd" 'chicken-doc)))

;;;; paredit

(add-to-list 'load-path "~/.emacs.d/paredit")
(autoload 'paredit-mode "paredit"
 "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
;; paredit-*-list became paredit-*-parenthesis in paredit-20.
(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd ")")   'paredit-close-parenthesis)  ;; Swap ) and M-)
          (define-key paredit-mode-map (kbd "M-)") 'paredit-close-parenthesis-and-newline)
	  ;; Terminal doesn't like C-) etc., so rebind frequently-used commands below.
	  (define-key paredit-mode-map "\C-c)"     'paredit-forward-slurp-sexp)
	  (define-key paredit-mode-map "\C-c}"     'paredit-forward-barf-sexp)
	  (define-key paredit-mode-map "\C-c("     'paredit-backward-slurp-sexp)
	  (define-key paredit-mode-map "\C-c{"     'paredit-backward-barf-sexp)
	  (define-key paredit-mode-map "\M-R"      'paredit-splice-sexp-killing-backward)
          ))

;;;; SLIME

;; Startup untested.  Disabled since unused for extended period of time.
;; Consider installing in standard emacs path.

;;(add-to-list 'load-path (expand-file-name "~/lisp/slime"))
;;(autoload 'slime "slime")
;;(eval-after-load 'slime
;;  '(progn (slime-setup)
;;	  (setq inferior-lisp-program (expand-file-name "~/local/bin/openmcl"))))

;;;; eggdoc

;; I don't think this can be activated buffer-local.
;; Additionally, M-x enable-eggdoc-indent is not found, must use M-: .
(defconst *eggdoc-indent-keywords* '(subsection section record procedure parameter))
(defun indent-scheme-keywords (keywords level)
  (mapc #'(lambda (name)
	    (put name 'scheme-indent-function level))
	keywords))
(defun enable-eggdoc-indent ()
  (indent-scheme-keywords *eggdoc-indent-keywords* 1))
(defun disable-eggdoc-indent ()
  (indent-scheme-keywords *eggdoc-indent-keywords* nil))
(defun copy-scheme-indent (from to)
  (put to 'scheme-indent-function 
       (get from 'scheme-indent-function)))
(defun copy-scheme-indents (indents)
  (mapc #'(lambda (x) (copy-scheme-indent (car x) (cadr x)))
	indents))

;;;;; Always indented keywords
;; This is here so it appears after indent-scheme-keywords is defined.
(indent-scheme-keywords '(and-let* let-location match-let) 1)
(indent-scheme-keywords '(handle-exceptions with-renamed foreign-lambda*) 2)
;; These are for ER-macros
(copy-scheme-indents 
 '((begin %begin) (let %let) (lambda %lambda) (case %case) (cond %cond)
   (foreign-lambda* %foreign-lambda*)
   ))
(copy-scheme-indents
 '((let let-prepare) (call-with-input-file call-with-database)
   (with-input-from-file with-transaction)
   ))
;; special case for ER defines; see scheme-indent-function in scheme.el
(put '%define 'scheme-indent-function 'defun)
;; dedent modules to column 0
(defun scheme-module-indent (state indent-point normal-indent) 0)
(eval-after-load 'scheme
 ;; May want to move other indent assigns to eval-after-load.
 '(put 'module 'scheme-indent-function 'scheme-module-indent))

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

(autoload 'company-ghci "company-ghci" nil t)
;; Most of these are not Haskell-mode specific, and should be moved to the company-mode section.
(with-eval-after-load 'company
  (push 'company-ghci company-backends)
  (setq company-idle-delay 0.2) ;; Complete fast enough to avoid need for company-complete binding
  ;; Use TAB to also cycle through completions instead of just completing common part.
  ;; Disallow RET from completing selection, as I often hit RET at EOL.
  ;; Move completion of selection to M-/ (M- already held if using M-n/M-p.)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map "M-/" 'company-complete-selection)
  ;; Alternate behaviors: 1) have TAB complete selection, do not allow complete common (like Atom).
  ;;  2) have TAB complete common, then complete selection instead of cycling. Would need a custom
  ;;     function for this.
  ;; TODO: Consider raising company-idle-delay back to 0.5 and allowing completion begin on TAB, for
  ;; less popup distraction. This may impact indentation.

  (setq company-dabbrev-downcase nil) ;; Prevent lowercasing identifiers in comments.
  (setq company-require-match nil) ;; Allow any non-match character to break out of completion.
  ;; Use M-/ to complete a word in code that hasn't been loaded yet. Needed because
  ;; dabbrev-code only lets you complete valid identifiers, and it's not valid until
  ;; saved and loaded into the interactive session. May no longer be valid now that we have
  ;; moved company-complete-selection to M-/.

  )  ; Assume ok to activate this globally. See [limit backend] below.
(add-hook 'haskell-mode-hook
          (lambda ()
            (auto-complete-mode 0)         ; interferes with company-mode
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
            )
          )
(add-hook 'haskell-interactive-mode-hook
          (lambda ()
            (company-mode)))

(with-eval-after-load 'haskell         ;; not 'haskell-mode
  ;; Normally 'haskell-process-cabal-build. Remove if we start using Cabal.                                            ;; Use C-u C-c C-c to change the compilation command; you can remove -c to build an executable.
  ;; (Then press 'g' in the compilation buffer to rebuild with same options.)
  (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  )

;; (add-hook 'haskell-interactive-mode-hook 'company-mode)  ; should be same hook as haskell-mode, actually

;; [limit backend] It's also possible to limit the backend to particular buffers.
;; ;; (add-hook 'haskell-mode-hook              ;; and 'haskell-interactive-mode-hook ?
;; ;;        (lambda () (setq-local company-backends '((company-ghci)))))
;; ;; (add-hook 'haskell-mode-hook              ;; alternate form of setq-local recommended by emacswiki
;; ;;        (lambda () (set (make-local-variable 'company-backends) '(company-ghci))))

;;;;; company-mode

;; Currently, company-mode is only used for Haskell.
;; It seems <f1> (company-doc) doesn't work -- the doc buffer is empty.

(autoload 'company-mode "company" nil t)

;; (add-hook 'after-init-hook 'global-company-mode)   ; Uncomment to use company-mode in all buffers

;; FAIL: company-quickhelp. Does not work on terminal, as pos-tip underlying library
;; (which ultimately uses built-in function x-show-tip) requires a GUI.
;; autocomplete's popup.el works fine on terminal.


;;;; python

(add-hook 'python-mode-hook
          (lambda ()
            (idle-highlight-mode t)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; We can change elpy from python2 to python3 (clunkily) by setting the rpc-virtualenv-path.

;;   (setq elpy-rpc-virtualenv-path "~/.emacs.d/elpy/rpc-venv3")

;; flake8 in RPC venv is not used -- the one in your path is. That is
;; either the current venv, or the --user or global env. This is a problem if you
;; have Py2 flake8 installed for --user and want to use Py3.
;; You can override elpy-syntax-check-command to use the rpc venv like:

;;   (setq elpy-syntax-check-command (concat elpy-rpc-virtualenv-path "/bin/" "flake8"))

;; although now it won't use the one in your current venv.

;; elpy will automatically create this venv (but at creation time only, you must set rpc-python-command to python3).
;;   (setq elpy-rpc-python-command "python3")                         ;; only needed at venv creation time
;; You can override python-shell-interpreter to 'python3' but 'python' is ok as long as you 'pyvenv-activate' into a python3 venv for your program.

;; M-x elpy-rpc-restart
;; M-x pyvenv-activate   (./venv)

;;;; sepia

;; Note: this modification is loaded globally

;; Slight modification to beginning-of-defun-raw from lisp.el --
;; prevents recursive call to this function when beginning-of-defun-function
;; calls beginning-of-defun.  This may be fixed in recent CVS Emacs.

(defun beginning-of-defun-raw (&optional arg)
  "Move point to the character that starts a defun.
This is identical to function `beginning-of-defun', except that point
does not move to the beginning of the line when `defun-prompt-regexp'
is non-nil.

If variable `beginning-of-defun-function' is non-nil, its value
is called as a function to find the defun's beginning."
  (interactive "p")
  (if beginning-of-defun-function
      (let ((fn beginning-of-defun-function)
	    (beginning-of-defun-function nil))
       (if (> (setq arg (or arg 1)) 0)
	   (dotimes (i arg)
	     (funcall fn))
	 ;; Better not call end-of-defun-function directly, in case
	 ;; it's not defined.
	 (end-of-defun (- arg))))
    (and arg (< arg 0) (not (eobp)) (forward-char 1))
    (and (re-search-backward (if defun-prompt-regexp
				 (concat (if open-paren-in-column-0-is-defun-start
					     "^\\s(\\|" "")
					 "\\(?:" defun-prompt-regexp "\\)\\s(")
			       "^\\s(")
			     nil 'move (or arg 1))
	 (progn (goto-char (1- (match-end 0)))) t)))

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

(setq vc-handled-backends nil)   ;; Use dedicated packages (like magit) for version control.

;;;; Gforth

(autoload 'forth-mode "gforth" "Major mode for editing Forth code." t)
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
     			    auto-mode-alist))
(autoload 'forth-block-mode "gforth" "Major mode for editing Forth blocks." t)
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
     			    auto-mode-alist))
(add-hook 'forth-mode-hook 
	  (function (lambda ()
		      ;; customize variables here:
		      (setq forth-indent-level 4)
		      (setq forth-minor-indent-level 2)
		      (setq forth-hilight-level 3)
		      (setq indent-tabs-mode nil)
		      (define-key forth-mode-map "\C-m" 'newline-and-indent)
		      )))
(add-hook 'inferior-forth-mode-hook
	  (lambda ()
	    (setq comint-process-echoes t)))

;;;; winring
;; bug: when creating a new frame, current window config name will be
;; changed to newly generated name for the new frame

;; --- I never use winring.  Multiple frames seem to suffice.
;; (require 'winring)             ;; C-x 7 C-h for help, or see winring.el
;; (winring-initialize)
;; (setq winring-show-names t)

;;;; Javascript

;; Using new espresso mode.  Mihai's JS mode is deprecated;
;; js2 mode is stupid.  espresso mode may be in emacs source
;; in the future as 'js-mode'.

(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(defun my-espresso-mode-hook ()
  (setq espresso-indent-level 2)
  (setq indent-tabs-mode nil))

(eval-after-load 'espresso
  '(progn (define-key espresso-mode-map "\C-m" 'newline-and-indent)
	  ;; fixes problem with pretty function font-lock (?)
	  ;; (define-key espresso-mode-map (kbd ",") 'self-insert-command)
	  (font-lock-add-keywords
	   'espresso-mode `(("\\(function *\\)("   ;; anonymous fxns -> ƒ
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "ƒ")
				       nil)))))
	  ;; JFU override because original did not move cursor correctly after
	  ;; an indent in a line which had a composed region.  We use
	  ;; move-to-column instead of forward-char.
	  (defun espresso-indent-line ()
	    "Indent the current line as JavaScript source text."
	    (interactive)
	    (save-restriction
	      (widen)
	      (let* ((parse-status
		      (save-excursion (syntax-ppss (point-at-bol))))
		     (offset (- (current-column) (current-indentation))))
		(indent-line-to (espresso--proper-indentation parse-status))
		(when (> offset 0)  ;; original used forward-char here
		  (move-to-column (+ (current-indentation) offset))))))
))

(add-hook 'espresso-mode-hook 'moz-minor-mode)
(add-hook 'espresso-mode-hook 'my-espresso-mode-hook)

;; ;; To connect to running JS Shell Server in Firefox, use M-x js-mode, M-x js-connect.
;; (autoload 'js-mode "js-mode" "JavaScript Shell mode." t)

;; ;;;; MozRepl
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; ;; Bugfix for moz-send-region -- the code sends as 'multiline' but this seems to
;; ;; crash the REPL almost every time.  instead we just send as-is, and the send data
;; ;; doesn't appear in the REPL anyway.  Note: this function is currently inserted
;; ;; directly into moz.el as we cannot hook the minor mode.
;; ;; (defun moz-send-region (start end)
;; ;;   (interactive "r")
;; ;;   (comint-send-region (inferior-moz-process)
;; ;;                       start end)
;; ;;   (display-buffer (process-buffer (inferior-moz-process))))

;; ;; Bugfix 2: inferior-moz-switch-to-mozilla requires a prefix argument.  
;; ;; Again, change was made directly to source.  Both bugfixes were required as
;; ;; of 2008-06-16.
;; ;; (defun moz-send-defun-and-go (arg)
;; ;;   (interactive "P")
;; ;;   (moz-send-defun)
;; ;;   (inferior-moz-switch-to-mozilla arg))


;;;; css-mode

;; Warning: Disabled; this code appears to be for the css-mode by
;; Lars Marius Garshol, while the currently-loaded version is
;; by Alex Schroder.
;; (add-hook 'css-mode-hook
;; 	  (lambda ()
;;             (setq cssm-indent-function #'cssm-c-style-indenter)  ; or else indentation is bizarre
;; 	    (setq cssm-indent-level 4)))
;; (eval-after-load 'css-mode
;; ;; 	    (define-key cssm-mode-map "\M-/" 'cssm-complete-property) ; overrides regular dabbrev; could use TAB
;;  	    (define-key cssm-mode-map "\t" 'cssm-complete-property)
;; 	    (define-key cssm-mode-map "\C-m" 'newline-and-indent)
;; 	    (define-key cssm-mode-map "\C-j" 'newline))

	  
;;;; csv-mode

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;(autoload 'csv-mode "csv-mode"
;  "Major mode for editing comma-separated value files." t)
(setq csv-header-lines 1)    ;; Assume all csv files have a header, for auto region select

;;;; w3m

(autoload 'w3m-mode "w3m" "w3m-mode" t)

;;;; html-mode

(defun html-tidy-buffer ()
  (interactive)
  (mark-whole-buffer)
  (shell-command-on-region (point-min) (point-max)
			   "tidy -w 0 -xml -q -i" 
			   t   ; other output buffer (n/a)
			   t   ; replace
  ))

;;;; ifm-mode

;; -- NB If re-enabled, please convert to autoload.
;; (require 'ifm-mode)     ;; Major mode for editing Interactive Fiction Mapper (IFM) maps
;; ;; Use C-c C-m to rerun ifm.
;; ;; Wrapper around Preview.app.  Preview will pop up but you
;; ;; must hit Revert to update (I set this to Command-R in System Preferences).
;; (setq ifm-viewer "~/local/bin/ifm-view")

;;;; dired-mode

;; Prefer 'gls' for dired, otherwise fall back to ls.
(let ((gnu-ls (executable-find "gls")))
  (if gnu-ls
      (setq insert-directory-program gnu-ls)))

(put 'dired-find-alternate-file 'disabled nil)   ; use 'a' to reuse dir buffer
(define-key global-map "\C-x\C-j" 'dired-jump)   ; allow dired-jump before dired is loaded
(autoload 'dired-jump "dired" "Load dired when dired-jump is used." t)

;; Load Dired-x when Dired is loaded.
(add-hook 'dired-load-hook
	  (lambda () 
	    (require 'dired-x)
	    ;; set dired-x global vars here
	    (define-key dired-mode-map "o" 'dired-open-mac)  ; use 'o' to /usr/bin/open files
	    ))
(setq default-dired-omit-mode 0)   ; 1 for omit by default, 0 for non-omit mode
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; set dired-x buffer-local vars here
 	    (dired-omit-mode default-dired-omit-mode)
	    ;; use M-o to toggle omit
	    ))

;; Enter virtual dired mode on files ending with .dired.  Useful for
;; saving a dired buffer for later or for importing an ls -lR or
;; find listing from the command line.
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
			    auto-mode-alist))

;; Jump to directory of current buffer and kill the existing buffer--
;; similar to C-x C-v C-k RET.  Author: yours truly.
;; Evidently unused.
(defun dired-kill-and-jump ()
  (interactive)
  (let ((oldbuf (current-buffer)))
    (dired-jump)
    (kill-buffer oldbuf)))

;; Open file using /usr/bin/open.
(defun dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
	(call-process "/usr/bin/open" nil 0 nil file-name))))

;;;;; Quickly browse through files in dired (http://stackoverflow.com/a/19933337)
;; Fixes: dired-next-line, dired-previous-line require an argument
;; Changes: use 'v' to view (not ^o, which we expect to edit in another window)

(add-hook 'dired-mode-hook
  (lambda()
    (define-key dired-mode-map (kbd "v") 'dired-view-current)     ; was dired-display-file
    (define-key dired-mode-map (kbd "n")   'dired-view-next)           ; was dired-next-line
    (define-key dired-mode-map (kbd "p")   'dired-view-previous))) ; was dired-previous-line

(defun dired-view-next ()
  "Move down one line and view the current file in another window."
  (interactive)
  (dired-next-line 1)
  (dired-view-current))

(defun dired-view-previous ()
  "Move up one line and view the current file in another window."
  (interactive)
  (dired-previous-line 1)
  (dired-view-current))

(defun dired-view-current ()
  "View the current file in another window (possibly newly created)."
  (interactive)
  (if (not (window-parent))
      (split-window-horizontally
       ))                                   ; create a new window if necessary
  (let ((file (dired-get-file-for-visit))
        (dbuffer (current-buffer)))
    (other-window 1)                                          ; switch to the other window
    (unless (equal dbuffer (current-buffer))                 ; don't kill the dired buffer
      (if (or view-mode (equal major-mode 'dired-mode))   ; only if in view- or dired-mode
          (kill-buffer)))                                                    ; ... kill it
    (let ((filebuffer (get-file-buffer file)))
      (if filebuffer                              ; does a buffer already look at the file
          (switch-to-buffer filebuffer)                                    ; simply switch 
        (view-file file))                                                    ; ... view it
      (other-window -1))))                   ; give the attention back to the dired buffer

;; https://stackoverflow.com/a/18885461/4289268
;; Note 'dired-create-empty-file' will be available in emacs 27.1.
(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "_") 'my-dired-create-empty-file)
     (defun my-dired-create-empty-file (file)
       "Create a file called FILE.
If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))

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

;;;; scheme-complete

;; Disabled--I never use scheme-complete..
;; (autoload 'scheme-complete-or-indent "scheme-complete" nil t)
;; (eval-after-load 'scheme
;;  '(define-key scheme-mode-map [(tab)] 'scheme-complete-or-indent))

;; Repository pathname required, and auto-detect (using 'csi') fails
;; as our interpreter is called csi4.
;; 2010-01: Caused a weird problem when repository bumped to chicken/5,
;;          so removing.  We use "csi" for Chicken 4 now anyway.
;; (setenv "CHICKEN_REPOSITORY" "/Users/jim/local/chicken-4/lib/chicken/4")


;;;; desktop

(require 'desktop-menu)               ;; implicitly loads 'desktop
(defalias 'dm 'desktop-menu)
(setq desktop-menu-directory "~/.emacs.d/desktops")
(setq desktop-load-locked-desktop t)  ;; Locking unusable unless we implement desktop-menu-kill. (see below)
(setq desktop-menu-clear t)           ;; Clear unconditionally.  'ask' is annoying and using 'x' from menu to clear beforehand also kills the menu!
;;(setq desktop-menu-autosave 120)    ;; autosave after # of seconds, or t to autosave at exit; default nil (no saving)

;; desktop-menu issues:
;;  Must press "q" to quit desktop-menu in order to save desktop list.  Otherwise
;;    desktop name won't be saved.
;;  Saves files like '.emacs.desktopN' instead of 'name.emacs.desktop', strangely.
;;  Always locks .emacs.desktop.lock regardless of desktop you load, so every desktop
;;    load will give you a lock warning.  [Fixed by me to lock specific desktop.]
;;  Doesn't unlock desktop on exit.  desktop-kill will do this (in fact it is done
;;    automatically in kill-emacs-hook) but knows nothing about our multiple desktops;
;;    if desired, add desktop-menu-kill which would use desktop-menu--current-desktop to set
;;    desktop-dirname and desktop-base-{file,lock}-name and call desktop-kill.
;;  Will often leave several minibuffer windows open.
;;  L (list buffers) appends to *desktop buffer list* instead of clearing first.
;;    Workaround: use 'q' to exit desktop buffer list.

;; NB: customization is in customize-group desktop-menu

;;;; ocaml

;; Requires MELPA packages tuareg, utop and merlin; also `opam install
;; merlin`. We use company-mode now as auto-complete is unmaintained,
;; though the latter still works fine.
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'merlin-mode-hook 'company-mode)

;; utop phrase eval doesn't seem to work (consistently evaluates much
;; smaller regions than expected.)  May be that tuareg-discover-phrase
;; is broken.
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;; Use the opam installed utop. Doesn't seem necessary.
(setq utop-command "opam config exec -- utop -emacs")
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; Note: If you use 'easy setup mode, which turns off automatic completion
;; for some reason, you will probably want to set a trigger key via
;;    (ac-set-trigger-key "TAB"), or bind auto-complete to M-tab or C-c <tab>.
;; Alternatively just use 't mode to use your standard auto-complete config.
;; Warning: Must manually autoload auto-complete-mode, or (setq auto-complete-mode t)
;; in merlin-mode-hook, as merlin-ac will only be loaded after auto-complete is.
;(setq merlin-ac-setup t) 

(eval-after-load 'merlin
  '(progn
     ;; Can't get this to work regardless of what key it's on ... it just seems to
     ;; cancel the type enclosing query. It worked once, but never again.
     (define-key merlin-mode-map (kbd "M-<up>") 'merlin-type-enclosing-go-up)
     (define-key merlin-mode-map (kbd "M-<down>") 'merlin-type-enclosing-go-down)
     ;; merlin-document is useful and not bound by default. Also see C-c C-l.
     (define-key merlin-mode-map (kbd "C-c d") 'merlin-document)  ; or C-c m d
     ;; merlin-jump may also be worth binding. Try C-c m j, or C-c C-j.
     ))
             
;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)

;;;; artist

(eval-after-load 'artist
  '(progn
     ;; Can't get rebind of s-mouse-1 to mouse-2 to work with artist mode, so manually
     ;; rebind the context menu from mouse-2 to s-mouse-1.
     (define-key artist-mode-map (kbd "<s-mouse-1>") 'artist-mouse-choose-operation)    ; Cmd-click for middle click
     ))

;;;; rust-mode

(use-package which-key
  :config
  (which-key-mode t))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "M-l")  ;; overrides downcase-word
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :hook (rust-mode . lsp))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))
  
(use-package rust-mode)

;;;; bookmarks
(setq bookmark-default-file (locate-user-emacs-file "bookmarks"))
(setq bookmark-save-flag 1)
;;;; tramp

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; Custom

(custom-reset-variables            ; what the heck does this do
 '(org-allow-space-in-links nil))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load-file custom-file)

;;; Random crap
;; insert test1 .. test5 into buffer
;; (mapcar (lambda (x) (insert "test" (format "%d" x) "\n")) '(1 2 3 4 5))
;; (let ((line 1)) (while (<= line 5) (insert "test" (format "%d" line) "\n") (setq line (+ line 1))))

;; repeat last command:             C-x z   (z to repeat further),   M-x repeat
;; repeat minibuffer command:       C-x M-:  or C-x ESC ESC  or M-x repeat-complex-command (like M-x M-p, but keeps arguments, as a lisp expression)
;; show minibuffer command history: M-x command-history
;; show last 100 chars typed:       M-x view-lossage, C-h l

;;; Aquamacs

(when (featurep 'aquamacs)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq x-select-enable-clipboard t)

;;(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))  ;; open *help* in current frame

  ;; Alternative to (setq Man-notify-method 'newframe).  'newframe
  ;; will open a new frame every single time, even if a frame exists
  ;; displaying that man page (this is arguably a bug).  Instead we
  ;; set to 'aggressive mode to open it in a new -window-, then tell
  ;; Aquamacs to open all '*Man ' buffers in their own window.
  ;; Now M-x man and iswitchb (C-x b) operate correctly but
  ;; C-x C-b (bs-mode) does not--maybe a config option.
  (add-to-list 'special-display-regexps '("[ ]?\\*Man .*" (width . 78) (height . 40)))
;  (add-to-list 'special-display-regexps '("[ ]?\\*Man .*" man-display-buffer-in-man-frame))
    ; marks as dedicated -- I think this conflicts with our goals


  (setq Man-notify-method 'aggressive)

  ;; Transparency
  (setq transparency-level 94)
  (set-frame-parameter nil 'alpha transparency-level)  ; set alpha on current frame
  (add-hook 'after-make-frame-functions                ; set alpha on future frames
	    (lambda (selected-frame)
	      (set-frame-parameter selected-frame 'alpha transparency-level)))


  )
