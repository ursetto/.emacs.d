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

(provide 'init-org)
