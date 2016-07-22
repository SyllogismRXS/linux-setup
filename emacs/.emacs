;;; .emacs --- Sample user customizations for Emacs EDT emulation

;; Copyright (C) 1986, 1992, 1993, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Kevin Gallagher <kgallagh@@spd.dsccc.com>
;; Maintainer: Kevin Gallagher <kgallagh@@spd.dsccc.com>
;; Keywords: emulations

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an example of the `edt-user.el' file that you can use
;; to customize the Emacs EDT emulation mode.  Copy this file to
;; somewhere in your `load-path', and edit it as desired.
;; See Info node `edt' for more details.

;; ====================================================================

;;; Code:

;;;;
;;;; Setup user custom EDT key bindings.
;;;;

(defun edt-setup-user-bindings ()
  "Assigns user custom EDT Emulation keyboard bindings."

  ;; PF1 (GOLD), PF2, PF3, PF4
  ;;
  ;; This file MUST contain a binding of PF1 to edt-user-gold-map.  So
  ;; DON'T CHANGE OR DELETE THE REGULAR KEY BINDING OF PF1 BELOW!
  ;; (However, you may change the GOLD-PF1 binding, if you wish.)
  (edt-bind-function-key "PF1" 'edt-user-gold-map 'edt-mark-section-wisely)
  (edt-bind-function-key "PF2" 'query-replace 'other-window)
  (edt-bind-function-key "PF4" 'edt-delete-entire-line 'edt-undelete-line)

  ;; EDT Keypad Keys
  (edt-bind-function-key "KP1" 'edt-word-forward 'edt-change-case)
  (edt-bind-function-key "KP3" 'edt-word-backward 'edt-copy)
  (edt-bind-function-key "KP6" 'edt-cut-or-copy 'yank)
  (edt-bind-function-key "KP8" 'edt-scroll-window 'fill-paragraph)
  (edt-bind-function-key "KP9" 'open-line 'edt-eliminate-all-tabs)
  (edt-bind-function-key "KPP"
			 'edt-toggle-select 'edt-line-to-middle-of-window)
  (edt-bind-function-key "KPE" 'edt-change-direction 'overwrite-mode)

  ;; GOLD bindings for regular keys.
  (edt-bind-gold-key "a" 'edt-append)
  (edt-bind-gold-key "A" 'edt-append)
  (edt-bind-gold-key "h" 'edt-electric-user-keypad-help)
  (edt-bind-gold-key "H" 'edt-electric-user-keypad-help)

  ;; Control bindings for regular keys.
  ;;; Leave binding of C-c as original prefix key.
  (edt-bind-key "\C-j" 'edt-duplicate-word)
  (edt-bind-key "\C-k" 'edt-define-key)
  (edt-bind-gold-key  "\C-k" 'edt-restore-key)
  (edt-bind-key "\C-l" 'edt-learn)
  ;;; Leave binding of C-m to newline.
  (edt-bind-key "\C-n" 'edt-set-screen-width-80)
  (edt-bind-key "\C-o" 'open-line)
  (edt-bind-key "\C-p" 'fill-paragraph)
  ;;; Leave binding of C-r to isearch-backward.
  ;;; Leave binding of C-s to isearch-forward.
  (edt-bind-key "\C-t" 'edt-display-the-time)
  (edt-bind-key "\C-v" 'redraw-display)
  (edt-bind-key "\C-w" 'edt-set-screen-width-132)
  ;;; Leave binding of C-x as original prefix key.
  )

;;;
;;; LK-201 KEYBOARD USER EDT KEYPAD HELP
;;;

(defun edt-user-keypad-help ()
  "
                                USER EDT Keypad Active

                                +----------+----------+----------+----------+
   F7: Copy Rectangle           |Prev Line |Next Line |Bkwd Char |Frwd Char |
   F8: Cut Rect Overstrike      |   (UP)   |  (DOWN)  |  (LEFT)  | (RIGHT)  |
 G-F8: Paste Rect Overstrike    |Window Top|Window Bot|Bkwd Sent |Frwd Sent |
   F9: Cut Rect Insert          +----------+----------+----------+----------+
 G-F9: Paste Rect Insert
  F10: Cut Rectangle
G-F10: Paste Rectangle
  F11: ESC                      +----------+----------+----------+----------+
  F12: Beginning of Line        |   GOLD   |Query Repl|  FNDNXT  |Del Ent L |
G-F12: Delete Other Windows     |   (PF1)  |   (PF2)  |   (PF3)  |   (PF4)  |
  F13: Delete to Begin of Word  |Mark Wisel|Other Wind|   FIND   |  UND L   |
 HELP: Keypad Help              +----------+----------+----------+----------+
G-HELP: Emacs Help              |   PAGE   |Scroll Win|Open Line |  DEL W   |
   DO: Execute extended command |    (7)   |    (8)   |    (9)   |   (-)    |
  C-a: Beginning of Line        |Ex Ext Cmd|Fill Parag|Elim Tabs |  UND W   |
  C-b: Backward Character       +----------+----------+----------+----------+
  C-d: Delete Character         |  ADVANCE |  BACKUP  | CUT/COPY |  DEL C   |
  C-e: End of Line              |    (4)   |    (5)   |    (6)   |   (,)    |
  C-f: Forward Character        |   BOTTOM |    TOP   |   Yank   |  UND C   |
  C-g: Keyboard Quit            +----------+----------+----------+----------+
G-C-g: Keyboard Quit            | Fwd Word |    EOL   | Bwd Word |  Change  |
  C-h: Electric Emacs Help      |    (1)   |    (2)   |    (3)   | Direction|
G-C-h: Emacs Help               | CHNGCASE |  DEL EOL |   COPY   |          |
  C-i: Indent for Tab           +---------------------+----------+  (ENTER) |
  C-j: Duplicate Word           |         LINE        |SELECT/RES|          |
  C-k: Define Key               |         (0)         |    (.)   |  Toggle  |
G-C-k: Restore Key              |      Open Line      |Center Lin|Insrt/Over|
  C-l: Learn                    +---------------------+----------+----------+
  C-n: Set Screen Width 80
  C-o: Open Line                       +----------+----------+----------+
  C-p: Fill Paragraph                  |  FNDNXT  |   Yank   |    CUT   |
  C-q: Quoted Insert                   |  (FIND)) | (INSERT) | (REMOVE) |
  C-r: Isearch Backward                |   FIND   |          |   COPY   |
  C-s: Isearch Forward                 +----------+----------+----------+
  C-t: Display the Time                |SELECT/RES|SECT BACKW|SECT FORWA|
  C-u: Universal Argument              | (SELECT) |(PREVIOUS)|  (NEXT)  |
  C-v: Redraw Display                  |          |          |          |
  C-w: Set Screen Width 132            +----------+----------+----------+
  C-z: Suspend Emacs
G-C-\\: Split Window

  G-a: Append to Kill Buffer
  G-b: Buffer Menu
  G-c: Compile
  G-d: Delete Window
  G-e: Exit
  G-f: Find File
  G-g: Find File Other Window
  G-h: Keypad Help
  G-i: Insert File
  G-k: Toggle Capitalization Word
  G-l: Lowercase Word or Region
  G-m: Save Some Buffers
  G-n: Next Error
  G-o: Switch Windows
  G-q: Quit
  G-r: Revert File
  G-s: Save Buffer
  G-u: Uppercase Word or Region
  G-v: Find File Other Window
  G-w: Write file
  G-y: EDT Emulation OFF
  G-z: Switch to Default EDT Key Bindings
  G-2: Split Window
  G-%: Go to Percentage
  G- : Undo  (GOLD Spacebar)
  G-=: Go to Line
  G-`: What line
  G-/: Query-Replace"

  (interactive)
  (describe-function 'edt-user-keypad-help))


                                        ;(add-to-list 'load-path "~/.emacs.d/site-lisp/cc-mode")

                                        ;Set load-path for libraries (Linux Only)
                                        ;In windows, the site-lisp direction should be
                                        ;in the emacs-23.3/ directory, i.e. same level
                                        ; as /bin
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq inhibit-splash-screen t)

(require 'template)
(template-initialize)

(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)

                                        ; Use a different color scheme when using terminal
                                        ;(when (display-graphic-p) (color-theme-dark-laptop))

;; Modify the default grep command when I type: M-x grep
;; -n : list line numbers
;; -H : print the filename for each match
;; -e : useful to protect patterns beginning with -.
(setq grep-command "grep -nHir -e ")

;; Enable ido-mode
(require 'ido) ;enable ido-mode

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defun emacs-format-function ()
  "Format the whole buffer."
  (interactive)
  (c-set-style "k&r")
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (save-buffer)
  )

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
         (backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ⁖ “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
         )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
    )
  )

(setq make-backup-file-name-function 'my-backup-file-name)

;; setup for template package

(setq user-mail-address "kevin.demarco@gmail.com")
(setq user-website "http://www.kevindemarco.com")
(setq user-full-name "Kevin DeMarco")

;; Line numbers on side of screen
(require 'linum)
(line-number-mode 1)
(column-number-mode 1)  ;; Line numbers on left most column
                                        ;(global-linum-mode 1)
(setq linum-format "%4d \u2502")

;; Set timestamp
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-pattern nil)

                                        ; Use spaces instead of tabs
;(setq-default indent-tabs-mode nil)

(setq c-default-style "k&r")

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(setq-default fill-column 79)
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode t)))
(global-fci-mode t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Matlab-emacs setup
(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-emacs")
(load-library "matlab-load")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex for Emacs
;; 
;; Dependencies: okular, texlive-full, auctex
;;
;; Okular setup: 
;; 1.) Open Okular and go to...
;; 2.) Settings -> Configure Okular -> Editor
;; 3.) Set Editor to "Emacs client"
;; 4.) Command should automatically set to: 
;;             emacsclient -a emacs --no-wait +%l %f
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable the .ido.last question on exit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-command-list
   (quote
    (("XeLaTeX_SyncteX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run XeLaTeX")
     ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Jump to PDF" "%V" TeX-run-discard-or-function nil t :help "Run Viewer"))))
 '(TeX-modes
   (quote
    (tex-mode plain-tex-mode texinfo-mode latex-mode doctex-mode)))
 '(column-number-mode t)
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(package-selected-packages
   (quote
    (magit yasnippet org-journal markdown-mode flymake-cppcheck flycheck cpputils-cmake company-c-headers cmake-project auto-complete))))

                                        ;(latex-preview-pane-enable)

(setq-default TeX-master nil) ; Query for master file.
                                        ;(setq-default TeX-master "master") ; All master files called "master".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard emacs config (http://emacswiki.org/emacs/AUCTeX)
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode) ; enable auto-fill mode in latex

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography '("./*.bib"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; only start server for okular comms in latex mode
(add-hook 'LaTeX-mode-hook 'server-start) ;(server-start) 
(setq TeX-PDF-mode t) ;; use pdflatex instead of latex

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)

;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Okular as the pdf viewer. Build okular command, so that
;; Okular jumps to the current line in the viewer.
(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "okular --unique %o#src:%n%b")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun latex-word-count ()
  (interactive)
  (shell-command (concat "/usr/bin/texcount "
                         "uncomment then options go here "
                         (buffer-file-name))))



;; Org Mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(setq org-log-done 'time)

(setq org-tag-alist '(("idea" . ?i) ("leadlaser" . ?l) ("art" . ?a)))

(setq org-agenda-files (list "~/Dropbox/org/gtri.org"
                             "~/Dropbox/org/personal.org" 
                             "~/Dropbox/org/thesis.org"                             
                             ))

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored

(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED" "DEFERRED")))

;;; define categories that should be excluded
(setq org-export-exclude-category (list "google" "private"))

;;; Define the directory to which archive files should be saved
(setq org-archive-location "~/Dropbox/org/archive/%s_archive::")

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.
(defun org-mycal-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
                                        ; get categories
    (setq mycategory (org-get-category))
                                        ; get start and end of tree
    (org-back-to-heading t)
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
    (goto-char mystart)
                                        ; search for timerange
                                        ;(setq myresult (re-search-forward org-tstr-regexp myend t))
    (setq myresult t)
                                        ; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
                                        ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

;;; activate filter and call export function
(defun org-mycal-export ()
  "Exports Org Agenda to Google Calendar"
  (interactive)
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
    (org-export-icalendar-combine-agenda-files))
  (+ 1 2))

(setq org-icalendar-store-UID t)
                                        ;(setq org-icalendar-include-todo t)
(setq org-icalendar-use-scheduled '(event-if-not-todo todo-due))
(setq org-icalendar-use-deadline '(event-if-not-todo todo-due))

;; Debug / test functions
(defun doodlebug ()
  "Nonce function"
  (interactive)
  (message "Howdie-doodie fella"))

(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.world\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))


(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

(setq org-journal-dir "~/Dropbox/org/journal/")

(global-set-key [f5] 'recompile)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(add-to-list 'default-frame-alist '(height . 400))
(add-to-list 'default-frame-alist '(width . 100))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
        (delq (current-buffer) 
              (remove-if-not 'buffer-file-name (buffer-list)))))


(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(setq tramp-default-method "ssh")

                                        ; ;; scroll one line at a time (less "jumpy" than defaults)    
                                        ; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time    
                                        ; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
                                        ; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
                                        ; (setq scroll-step 1) ;; keyboard scroll one line at a time
                                        ; (setq scroll-conservatively 10000)

                                        ;(add-to-list 'company-backends 'company-c-headers)

                                        ;(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flymake)
(setq cppcm-write-flymake-makefile nil)

(add-hook 'after-init-hook 'global-company-mode)
                                        ;(require 'global-company-mode)
(eval-after-load 'company
  '(push 'company-c-headers company-backends))


(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))
;; OPTIONAL, somebody reported that they can use this package with Fortran
(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
                '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
;; OPTIONAL, some users need specify extra flags forwarded to compiler
(setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

;; Scroll to first error in compilation output
(setq compilation-scroll-output 'first-error)

;; Helper for compilation. Close the compilation window if there was no error
;; at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))   
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy from emacs to other programs:
; http://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
(defun my-copy-simple (&optional beg end)
  "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer 
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))


(setq yas-snippet-dirs
      '("~/repos/linux-setup/emacs/snippets"        
        ))      
(yas-global-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
