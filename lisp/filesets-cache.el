(setq-default filesets-be-docile-flag 'nil)

(setq filesets-submenus '("main" ("0 main" ["Files: main" (filesets-open (quote :files) (quote "main"))] "---" ["0 .emacs" (filesets-file-open nil (quote "/Users/jim/.emacs") (quote "main"))] ["1 to-read.txt" (filesets-file-open nil (quote "/Users/jim/doc/to-read.txt") (quote "main"))] "---" ["Close all files" (filesets-close (quote :files) (quote "main"))] ["Run Command" (filesets-run-cmd nil (quote "main") (quote :files))] ["Add current buffer" (filesets-add-buffer (quote "main") (current-buffer))] ["Remove current buffer" (filesets-remove-buffer (quote "main") (current-buffer))] ["Rebuild this submenu" (filesets-rebuild-this-submenu (quote "main"))])))

(setq filesets-menu-cache '(("0 main" ["Files: main" (filesets-open (quote :files) (quote "main"))] "---" ["0 .emacs" (filesets-file-open nil (quote "/Users/jim/.emacs") (quote "main"))] ["1 to-read.txt" (filesets-file-open nil (quote "/Users/jim/doc/to-read.txt") (quote "main"))] "---" ["Close all files" (filesets-close (quote :files) (quote "main"))] ["Run Command" (filesets-run-cmd nil (quote "main") (quote :files))] ["Add current buffer" (filesets-add-buffer (quote "main") (current-buffer))] ["Remove current buffer" (filesets-remove-buffer (quote "main") (current-buffer))] ["Rebuild this submenu" (filesets-rebuild-this-submenu (quote "main"))])))

(setq filesets-ingroup-cache 'nil)

(setq filesets-cache-version "1.8.4")

