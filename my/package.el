;; -*- outline-cycle-min-level: 2 -*-
;; package.el : M-x list-packages, M-x package-install; package-alist; package-activated-list
;;   Location : ~/.emacs.d/elpa/
;;   NOTE: silently discards packages that fail to load. (?)
;;   NOTE: When building 'org', had to use a clean emacs with org not loaded,
;;         or I got lots of errors during build.  Recommendation: 
;;         load just this file with `emacs -q -L ~/.emacs.d -l ~/.emacs.d/my/package.el`

;; Note Emacs 24.1 and higher load and init ELPA after your init file;
;; here I manually load and init it early, because I haven't vetted whether all
;; custom init in .emacs works prior to package load time.

;;; main

(require 'package)
;; add org ELPA repo (note that org base is included in marmalade repo as well)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; add marmalade ELPA repo
;; FIXME: Use https: --- once bug fixed in emacs 24.4 to allow direct https access
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib (recommended by MELPA)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(if (boundp 'package-enable-at-startup)
    (setq package-enable-at-startup nil))

(package-initialize)  ;; loads all packages in package-load-list; default "all"
