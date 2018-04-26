(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (setq package-list '(js2-mode))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(if (eq system-type 'gnu/linux)
    (progn
      (setq my-workspace-path "/home/andrea/Dropbox/org/")
      (setq org-agenda-files (list (concat my-workspace-path "agenda/agenda.org"))))
  (setq my-workspace-path "C:\\Users\\ANDREA\\Dropbox\\org\\")
  (setq org-agenda-files (list (concat my-workspace-path "agenda\\agenda.org"))))

;;; Start Emacs server
(require 'server)
(or (server-running-p) (server-start))

;;; Create key binding for auto fill mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;; Set advice for creating directory when saving a new file if it
;;; does not exist

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;;; miscellaneous options
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p) 
(set-fringe-mode '(0 . 1))
(follow-mode 1)

;; Set text-mode as default for new buffers
(setq default-major-mode 'text-mode)

;;; Remove toolbar and menu
(menu-bar-mode 0)
(tool-bar-mode 0)

;; use nxml-mode for html
;;;###autoload
(require 'nxml-mode)
(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
	   (eq (cdr pair) 'sgml-mode)
	   (eq (cdr pair) 'html-mode))
       (setcdr pair 'nxml-mode)))
 auto-mode-alist)

;; Custom function to center current frame on screen by using fringes
(setq my-fringe-default-color (face-attribute 'fringe :background))

(defun center-frame (arg)
  (interactive "P")
  (if (not arg)
      (let ((fringe-size
	     (/ (- (frame-pixel-width) 700) 2)))
	(set-fringe-style `(,fringe-size . ,fringe-size))
	(set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))
    (progn
      (set-fringe-mode '(0 . 1))
      (set-face-attribute 'fringe nil
			  :foreground (face-foreground 'default)
			  :background my-fringe-default-color)
    )))

;; Remove annoying closing of window when pressing C-z by mistake
;; and use it for undo instead
(global-set-key (kbd "C-z") 'undo)

;;; Setup sh in windows
(if (eq system-type 'windows-nt)
    (progn
      (setq explicit-shell-file-name "C:/Program Files (x86)/Git/bin/sh.exe")
      (setq shell-file-name "sh")
      (setq explicit-sh.exe-args '("--login" "-i"))
      (setenv "SHELL" shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
      )
  )1

;;; add folder from where load custom lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/screenplay-mode/")

;;; Needed to be able to insert ascii characters in decimal with C-q
(setq read-quoted-char-radix 10)

;;; Org mode
;;;###autoload
(require 'org-install)
;;;###autoload
(require 'org-habit)

;;; Arduino files
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . c-mode))

;;; Org mode keybindings.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; Set to open and empty scratch file at the beginning
;;(setq initial-buffer-choice 
;;      (concat default-directory "meditazioni.org"))
(setq initial-scratch-message "")

;;; Set emacs to show column number by default
(column-number-mode 1)

;;; Enable Ren'Py mode
;;;###autoload
(require 'renpy)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; enable screenplay mode
;;;###autoload
(require 'screenplay)

;; Calendar function
;; (load "calendarpage.el")

;;; save all backups in one folder
(setq backup-directory-alist '(("."."~/.emacs.d/saves")))

;;; Custom Keybindings
(global-set-key (kbd "C-x C-j") 'emmet-expand-line)

;;; Auto refresh directories in dired mode
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; inrease/decrease font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Set inconsolata as default font
(set-default-font "DejaVu Sans Mono-10")

;;; Search and replace, no query
(global-set-key (kbd "C-%") 'replace-string)

;;; Show date on mode bar
;; (display-time-mode 0)
;; (custom-set-variables '(display-time-24hr-format 1)
;; 		      '(display-time-day-and-date 1))

;;; Lunar phase functions for journaling purposes
(require 'calendar)
(require 'lunar)
(require 'cal-dst)

(defun my-lunar-phase-today ()
  (interactive)
  (let* ((date-today (calendar-current-date))
       (index (lunar-index date-today))
       (phase-date (car (lunar-phase index)))
       (phase 0)
       (exact 0)
       (lunar-names '("Nuova" "Crescente" "Primo quarto" "Gibbosa crescente"
		     "Piena" "Gibbosa calante" "Ultimo quarto" "Calante")))
  (while (calendar-date-compare (list phase-date) (list date-today))
    (setq phase (+ phase 1))
    (setq phase-date (car (lunar-phase (+ index phase)))))
  (setq phase (- phase 1))
  (setq phase-date (car (lunar-phase (+ index (- phase 1)))))
  (if (equal phase-date date-today)
      (setq exact 0)
    (setq exact 1))
  (nth (+ exact (* phase 2)) lunar-names)))

;;; Journaling functions

(defun andrea-insert-timestamp-with-time ()
  "Add current timestamp at point"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]"))
)

(defun andrea-insert-timestamp ()
  "Add current timestamp at point"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a]"))
)

(defun andrea-add-meditation-entry ()
  "Insert journaling entry for meditation at the end of the current file."
  (interactive)
  (let (my-point)
    (goto-char (point-max))
    (insert "* ")
    (andrea-insert-timestamp)(newline)
    (insert "- Ora inizio :: ")(andrea-insert-timestamp-with-time)
    (setq my-point (- (point) 6))(newline)
    (insert "- Ora fine :: ")(andrea-insert-timestamp-with-time)(newline)
    (insert "- Carta :: ")(newline)
    (insert "- I Ching :: ")(newline)
    (insert "- Tempo :: ")(newline)
    (insert "- Luna :: ")(insert (my-lunar-phase-today))(newline)
    (insert "- Cond. Fisiche :: ")(newline)
    (insert "- Cond. Mentali :: ")(newline)
    (insert "- 5 Riti Tibetani :: ")(newline)
    (insert "- Riti effettuati :: ")(newline)
    (insert "- Sensazioni :: ")(newline)
    (goto-char my-point)
    )
  )

(defun andrea-open-meditation-journal () 
  "Open meditative journal and insert new entry at the end"
  (interactive)
  (find-file (concat my-workspace-path "meditazioni.org"))
  (andrea-add-meditation-entry))
(defun andrea-open-acquisti () 
  (interactive)
  (find-file (concat my-workspace-path "acquisti.org")))
(global-set-key (kbd "C-c M-a") 'andrea-open-acquisti)
(defun andrea-add-exercise-entry ()
  "Insert journaling entry for exercise at the end of the current file."
  (interactive)
  (let (my-point)
    (goto-char (point-max))
    (insert "* ")
    (andrea-insert-timestamp)(newline)
    (insert "- Note :: ")(newline)
    (goto-char my-point)
    )
  )

(defun andrea-add-journal-entry ()
  "Insert journaling entry for meditation at the end of the current file."
  (interactive)
  (goto-char (point-max))
  (insert "* ")
  (andrea-insert-timestamp)(newline))

(defmacro defjournal (function-name 
		      journal-path 
		      key-combination 
		      add-journal-function)
  (list 'global-set-key (list 'kbd key-combination)
	(list 'defun function-name '(arg)
	      (list 'interactive '"P") 
	      (list 'find-file journal-path)
	      (list 'if 'arg '(end-of-buffer) (list add-journal-function)))))

(defjournal andrea-open-journal
  (concat my-workspace-path "giornale.org" )
  "C-c M-n" 
  andrea-add-journal-entry)

(defjournal andrea-open-dream-journal
  (concat my-workspace-path "sogni.org" )
  "C-c M-d" 
  andrea-add-journal-entry)

(defjournal andrea-open-books 
  (concat my-workspace-path "libri.org")
  "C-c M-l"
  end-of-buffer)

(defun andrea-open-writepad ()
  "Open writepad and go to the end of the file"
  (interactive)
  (let ()
    (find-file (concat my-workspace-path "writepad.org"))
    (goto-char (point-max))
    (org-timer-start)))

(global-set-key (kbd "C-c M-q") 'andrea-open-writepad)

;;; misc shortcuts
(global-set-key (kbd "<f8>") 'calc)
(global-set-key (kbd "<f1>") 'shell)
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq-default dired-omit-files "^\\.?#\\|^\\.[^.]")
(global-set-key (kbd "<f2>") (lambda () (interactive) (dired "~")))
(global-set-key (kbd "<f12>") 
  (lambda()(interactive)                               
    (switch-to-buffer (get-buffer-create "*scratch*"))))

;;; open init file
(defun my-open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Automatically load follow-mode
(follow-mode 1)

;;; set default timer for org-timer
(setq org-timer-default-timer 5)

;;; Word count function
(defun count-words-from-ampersand () 
  (interactive)
  (save-excursion
    (let ((p (point))) 
      (search-backward "&")
      (message (number-to-string (count-words-region (point) p))))))

(global-set-key (kbd "C-c M-c") 'count-words-from-ampersand)

;;; File editing
(defun my-is-last-line ()
  (if (> (+ 1 (line-end-position)) (point-max)) t))

(defun andrea-move-line-above ()
  "Moves current line above the previous one"
  (interactive)
  (if (not (eq (line-number-at-pos) 1))
      (let* ((point-from-start (- (point) (line-beginning-position)))
	     (line (delete-and-extract-region
		    (line-beginning-position)
		    (line-end-position))))
	(backward-delete-char 1 nil)
	(move-beginning-of-line nil)
	(newline)
	(previous-line)
	(let ((line-start (point)))
	  (insert line)
	  (goto-char (+ line-start point-from-start))))))

(defun andrea-move-line-below ()
  "Moves current line below the next one"
  (interactive)
  (if (not (my-is-last-line))
      (let* ((point-from-start (- (point) (line-beginning-position)))
	     (line (delete-and-extract-region
		    (line-beginning-position)
		    (line-end-position))))
	(delete-forward-char 1 nil)
	(move-end-of-line nil)
	(newline)
	(let ((line-start (point)))
	  (insert line)
	  (goto-char (+ line-start point-from-start))))))

(global-set-key (kbd "M-<up>")   'andrea-move-line-above)
(global-set-key (kbd "M-<down>") 'andrea-move-line-below)

(global-set-key (kbd "C-'") '(lambda () (interactive) 
			       (move-end-of-line nil)
			       (newline-and-indent)))
(global-set-key (kbd "M-'") '(lambda () (interactive)
			       (previous-line)
			       (move-end-of-line nil)
			       (newline-and-indent)))

;;; Create toc for articles
(defun my-make-toc ()
  (interactive)
  (goto-char (point-max))
  (search-backward "** ")
  (delete-char 3)
  (let* ((title (buffer-substring-no-properties (point) (line-end-position)))
	(title-no-spaces (replace-regexp-in-string " " "-" title)))
    (message title)
    (insert (concat "<a name=\"" title-no-spaces "\"></a>"))
    (goto-char (point-min))
    (insert (concat "<a href=\"#" title-no-spaces "\">" title "</a><br>"))))

;;; Elisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; Some custom functions for javascript
;;; use js2-mode for javascript files
;;;###autoload
(require 'js2-mode)
(fset 'javascript-mode 'js2-mode)

(fset 'js-insert-loop
      [tab ?f ?o ?r ?\( ?  ?v ?a ?r ?  ?i ?= ?0 ?\; ?  ?i ?< ?\; ?  ?i ?+ ?+ ?  ?\) ?  ?\{ return return tab ?\} tab up tab])

(fset 'js-insert-brackets
   [?\{ return return ?\} tab up tab])

(fset 'js-insert-function
   [tab ?f ?u ?n ?c ?t ?i ?o ?n ?  ?\( ?\) ?  ?\{ return return ?\} up down tab up tab])

(define-key js2-mode-map (kbd "C-.") 'js-insert-brackets)
(define-key js2-mode-map (kbd "C-,") 'js-insert-loop)
(define-key js2-mode-map (kbd "C-ò") 'js-insert-function)

;;; org-mode options
(setq org-log-done 'time)
(setq org-directory my-workspace-path)
(setq org-export-html-validation-link nil)

;;; Run auto-fill-mode when entering org mode
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Setup the site org project
;;;###autoload
(require 'ox-publish)
(setq org-publish-project-alist
      '(("bomba-retorica"
	 :base-directory "~/Dropbox/site"
	 :base-extension "org"
	 :publishing-directory "~/Dropbox/site/html-export"
	 :recursive nil
	 :drawers nil
	 :creator-info nil
	 :publishing-function org-html-publish-to-html
	 :headline-levels 2
	 :html-head nil
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :html-postamble nil
	 :html-head "<link href=\"style.css\" rel=\"stylesheet\">\
<link href=\"https://fonts.googleapis.com/css?family=Cousine|Goudy+Bookletter+1911\" rel=\"stylesheet\">" )))

;; Macro to change a script into functions for the game.
;; useless now for the most part since the engine didn't get developed further
(fset 'vn-make-script
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 115 97 121 40 34 19 58 32 right left backspace backspace 34 44 32 34 5 34 41 44 down] 0 "%d")) arg)))

(fset 'vn-insert-branch
      [?\C-e return ?: ?  ?c ?o ?n ?n ?e ?c ?t ?\( ?\[ return ?\] ?\) ?, tab ?\C-r ?: right left])
(fset 'vn-close-branch
      [?\C-e return tab ?\] ?\) ?, return ?: ?  ?c ?o ?n ?n ?e ?c ?t ?\( ?\[ tab ?\M-m])
(fset 'vn-insert-say
      [?\C-e return ?s ?a ?y tab ?\( ?n ?, ?  ?_ ?\( ?\" ?\" ?\) ?\) ?, left left left left])
(fset 'vn-insert-translation
      [?\C-s ?\" left ?_ ?\( ?\C-s ?\C-s ?\C-s return ?\)])
(fset 'vn-insert-menu
      [?\C-e return ?m ?e ?n ?u ?b ?o ?x ?\( ?\[ return return ?\] ?\) ?, tab up tab ?\{ ?t ?e ?x ?t ?: ?  ?" ?" ?, ?  ?a ?c ?t ?i ?o ?n ?: ?  ?\} ?, return tab ?\{ ?t ?e ?x ?t ?: ?, ?  ?a ?c ?t ?i ?o ?n ?: ?  ?\} ?\C-r ?t ?e ?x ?t ?: right right right right right ?  ?\" ?\" left])
(fset 'vn-insert-menu-button
   [?\C-e return ?\{ tab ?t ?e ?x ?t ?: ?  ?\" ?\" ?, ?  ?a ?c ?t ?i ?o ?n ?: ?  ?, ?  ?y ?: ?  ?\} ?, ?\C-r ?\" return])

(define-minor-mode vn-old-mode
  "Minor mode for editing scripts made for visualplay"
  :lighter " vn"
  :keymap (let ((map (make-sparse-keymap)))
	    ;(define-key map (kbd "C-à") 'vn-make-script)
	    (define-key map (kbd "C-à") 'vn-insert-branch)
	    (define-key map (kbd "M-à") 'vn-close-branch)
	    (define-key map (kbd "C-è") 'vn-insert-menu)
	    (define-key map (kbd "M-è") 'vn-insert-menu-button)
	    (define-key map (kbd "C-ù") 'vn-insert-say)
	    (define-key map (kbd "M-ù") 'vn-insert-translation)
	    map))

;; hide temporarily the mode line

(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive) 
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

(global-set-key (kbd "<f10>") 'toggle-mode-line)
(global-set-key (kbd "<f9>") 'center-frame)

;; for remapping alt to meta on mechanical keyboard
(setq x-alt-keysym 'meta)

(defun my-org-insert-inactive-timestamp (arg)
  (interactive "P")
  (if arg
      (insert (format-time-string "[%Y-%m-%d %a %H:%M]"))
    (insert (format-time-string "[%Y-%m-%d %a]"))))
  
;; useful functions for renpy

(fset 'vn-convert-script
      [?\C-a ?\C-s ?- left delete ?  ?\" ?\C-e ?\" ?\C-a tab down])
(fset 'vn-insert-char
      [?s ?h ?o ?w ?  ?  ?a ?t left left left tab])

(define-minor-mode vn-mode
  "Minor mode for editing scripts made for visualplay"
  :lighter " vn"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-à") 'vn-convert-script)
	    (define-key map (kbd "C-ò") 'vn-insert-char)
	    map))

(defun reading-mode (arg)
  (interactive "P")
  (if (not arg)
      (progn
	(delete-other-windows)
	(split-window-right)
	(split-window-right)
	(balance-windows)
	(follow-mode 1))
    (progn
      (delete-other-windows)
      (follow-mode -1))))
