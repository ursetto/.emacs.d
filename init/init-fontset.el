;;; Fontsets

;; Unused legacy stuff.
;; Set the font / window size here by setting initial-frame-alist.  Font name and
;; height should not be set in (custom-set-variables '(default (...))) or weird glitches will occur.
;(setq initial-frame-alist `((width . 118) (height . 71) (font . "fontset-fixedr12")))
;(setq initial-frame-alist `((width . 118) (height . 66) (font . "fontset-monaco")))
;(setq initial-frame-alist `((width . 118) (height . 77) (font . "fontset-fixedr11")))
(setq-default line-spacing 1)   ;; Required for fixeer11; font height is slightly too small
;; Height needs to be increased for line-spacing 1, even though there only 77 lines.  I guess
;; height is calculated as if line-spacing were 0.  This has a deleterious effect on
;; certain functions which position based on screen height, e.g. slowsplit.el; however
;; this would also fail with differing line-heights anyway.

;(setq initial-frame-alist `((width . 118) (height . 86) (font . "fontset-fixeer11o")))
   ; (internal-border-width . 2) (border-color . "#4e3831")))
   ; Can't get border to work on Aquamacs -- present but always black -- using fringe as a replacement instead.
;(setq default-frame-alist initial-frame-alist)  ;; Propagate font/window size to all frames.

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

(provide 'init-fontset)
