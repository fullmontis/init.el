(when (>= emacs-major-version 24)
  (require 'package)

  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
	       
					; list the packages you want
  (setq package-list '(js2-mode))
  
					; activate all the packages
					; (in particular autoloads)
  (package-initialize)

					; fetch the list of packages available 
  (unless package-archive-contents
    (package-refresh-contents))
  
					; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(if (eq system-type 'gnu/linux)
    (setq my-workspace-path "/home/andrea/Dropbox/org/")
  (setq my-workspace-path "C:\\Users\\ANDREA\\Dropbox\\org\\"))

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

;;; Remove toolbar and menu
(menu-bar-mode 0)
(tool-bar-mode 0)

;; set aliases for html mode
(fset 'html-mode 'nxml-mode)

;; Remove annoying closing of window when pressing C-z by mistake
;; and use it for undo instead
(global-set-key (kbd "C-z") 'undo)

;;; Setup sh in windows
(if (eq system-type 'windows-nt)
    (let () 
      (setq explicit-shell-file-name "C:/Program Files (x86)/Git/bin/sh.exe")
      (setq shell-file-name "sh")
      (setq explicit-sh.exe-args '("--login" "-i"))
      (setenv "SHELL" shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
      )
  )

;;; add folder from where load custom lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Set default package repositories
;; (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
;;                          ("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;; 			 ))

;;; Start Evil mode by default
; (package-initialize)
; (evil-mode 0)

;;; Needed to be able to insert ascii characters in decimal with C-q
(setq read-quoted-char-radix 10)

;;; Org mode
(require 'org-install)
(require 'org-habit)

;; set org agenda files
(setq org-agenda-files (list (concat my-workspace-path "agenda\\agenda.org")))

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

;; enable screenwriter mode
(require 'screenwriter)

;; Calendar function
;; (load "calendarpage.el")

;;; save all backups in one folder
(setq backup-directory-alist '(("."."~/.emacs.d/saves")))

;;; Custom Keybindings
(global-set-key (kbd "C-x C-j") 'emmet-expand-line)

;;; Auto refresh directories in dired mode
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; inrease/decrease font size
;;(global-set-key (kbd "C-+") 'text-scale-increase)
;;(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C--") 'undo)
(set-face-attribute 'default nil :height 100)

;;; Search and replace, no query
(global-set-key (kbd "C-%") 'replace-string)

;;; Show date on mode bar
; (display-time-mode 1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Journaling function

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
	(list 'global-set-key (list 'kbd key-combination) (list 'defun function-name '() (list 'interactive) 
	      (list 'find-file journal-path)
	      (list add-journal-function))))

(defjournal andrea-open-meditation-journal
  (concat my-workspace-path "meditazioni.org" )
  "C-c M-m" 
  andrea-add-meditation-entry)

(defjournal andrea-open-journal
  (concat my-workspace-path "giornale.org" )
  "C-c M-n" 
  andrea-add-journal-entry)

(defjournal andrea-open-writing-journal
  (concat my-workspace-path "giornale_scrittura.org" )
  "C-c M-z" 
  andrea-add-journal-entry)

(defjournal andrea-open-earnings-journal
  (concat my-workspace-path "guadagni.org" )
  "C-c M-g" 
  andrea-add-journal-entry)

(defjournal andrea-open-dream-journal
  (concat my-workspace-path "sogni.org" )
  "C-c M-d" 
  andrea-add-journal-entry)

(defjournal andrea-open-exercise-journal
  (concat my-workspace-path "allenamento.org" )
  "C-c M-e" 
  andrea-add-journal-entry)

(defjournal andrea-open-gratitude-journal
  (concat my-workspace-path "gratitudine.org" )
  "C-c M-r" 
  andrea-add-journal-entry)

(defjournal andrea-open-sexuality-journal
  (concat my-workspace-path "sexuality.org" )
  "C-c M-s" 
  andrea-add-journal-entry)

(defjournal andrea-open-comedy-journal
  (concat my-workspace-path "comedy.org" )
  "C-c M-a" 
  andrea-add-journal-entry)

(defjournal andrea-open-drawing-journal
  (concat my-workspace-path "drawing.org" )
  "C-c M-f" 
  andrea-add-journal-entry)

(defjournal andrea-open-wakeup-journal
  (concat my-workspace-path "wakeup.org" )
  "C-c M-k" 
  andrea-add-journal-entry)

(defjournal andrea-open-ideas-journal
  (concat my-workspace-path "ideas.org" )
  "C-c M-i" 
  andrea-add-journal-entry)

(defjournal andrea-open-forgive-journal
  (concat my-workspace-path "forgive.org" )
  "C-c M-v" 
  andrea-add-journal-entry)

(defjournal andrea-open-morning-pages-journal
  (concat my-workspace-path "morning_pages.org" )
  "C-c M-p" 
  andrea-add-journal-entry)

(defun andrea-open-writepad ()
  "Automatically save current buffer, open writepad and go to the end of the file"
  (interactive)
  (let ()
    (save-buffer)
    (find-file (concat my-workspace-path "writepad.org"))
    (goto-char (point-max))
    (org-timer-start)))

(global-set-key (kbd "C-c M-q") 'andrea-open-writepad)

;;; misc shortcuts
(global-set-key (kbd "<f8>") 'calc)
;;(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<f12>") 
  (lambda()(interactive)                               
    (switch-to-buffer (get-buffer-create "*scratch*"))))
(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<end>") 'end-of-buffer)

;;; Use nxml for html
(fset 'html-mode 'nxml-mode)

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File editing
(defun andrea-move-line-above ()
  "Moves current line above the upper one"
  (interactive)
  (let ((point-from-start (- (point) (line-beginning-position)))
	(line (delete-and-extract-region 
	       (line-beginning-position)
	       (min (+ 1 (line-end-position)) (point-max)))))
      (ignore-errors (previous-line))
      (move-beginning-of-line nil)
      (let ((line-start (point)))
	(insert line)
	(goto-char (+ line-start point-from-start)))))

(defun andrea-move-line-below ()
  "Moves current line above the upper one"
  (interactive)
  (let ((point-from-start (- (point) (line-beginning-position)))
	(line (delete-and-extract-region 
	       (line-beginning-position)
	       (min (+ 1 (line-end-position)) (point-max)))))
      (ignore-errors (next-line))
      (move-beginning-of-line nil)
      (let ((line-start (point)))
	(insert line)
	(goto-char (+ line-start point-from-start)))))

(global-set-key (kbd "M-<up>")   'andrea-move-line-above)
(global-set-key (kbd "M-<down>") 'andrea-move-line-below)

(global-set-key (kbd "C-'") '(lambda () (interactive) 
			       (move-end-of-line nil)
			       (newline-and-indent)))
(global-set-key (kbd "M-'") '(lambda () (interactive)
			       (previous-line)
			       (move-end-of-line nil)
			       (newline-and-indent)))

;;; Programming utility keystrokes
(defun andrea-indent-curly-brackets ()
  "Automatically insert curly brackets, and format them"
  (interactive)
  (insert "{")
  (newline)(newline)
  (insert "}")
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))

(global-set-key (kbd "C-.") 'andrea-indent-curly-brackets)

;;; A function to create a TOC and html body for an article 
;; (defun my-make-toc (from to)
;;   (interactive (if (use-region-p)
;; 		   (list (region-beginning) (region-end))
;; 		 (list (point-min) (point-max))))
;;   (let ((article  (buffer-substring-no-properties from to)))
;;     (switch-to-buffer (get-buffer-create (concat (buffer-name (current-buffer)) ".htm")))
;;     (insert article)
;;     (goto-char (point-max))
;;     (search-backward "** ")
;;     (delete-forward-char)(delete-forward-char)(delete-forward-char)
;;     (let ((title (buffer-substring-no-properties (point) (line-end-position)))
;; 	  (title-no-spaces (replace-regexp-in-string " " "-" title)))
;;       (insert (concat "<a name=\"" title-no-spaces "\"></a>"))
;;       (goto-char (point-min))
;;       (insert (concat "<a href=\"#" title-no-spaces "\">" title "</a><br>"))
;;       (goto-char (point-max)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some custom functions for javascript
;;; use js2-mode for javascript files
(fset 'javascript-mode 'js2-mode)
(require 'js2-mode)

(defun andrea-js-insert-function (name arguments)
  "Adds simple function constructor for JS"
  (interactive (list (read-string "Function name: ")
		     (read-string "Arguments: ")))
  (let ((parleft  (if (string-equal arguments "") "(" "( "))
	(parright (if (string-equal arguments "") ")" " )")))
    (insert (concat "function " name  parleft  arguments parright " {}"))
    (andrea-indent-brackets)))

(defun andrea-js-insert-array-iterator (var array)
  "Inserts a JS for loop that loops through all the existing 
elements in array"
  (interactive (list (read-string "Var name (default i): ")
		     (read-string "Array: ")))
  (let ((var (if (string-equal var "") "i" var)))
    (insert "for( var " var "=0; " var "<" array ".length; " var "++ ) {}")
    (andrea-indent-brackets)))

(define-key js2-mode-map (kbd "C-,") 'andrea-js-insert-array-iterator)
(define-key js2-mode-map (kbd "C-ò") 'andrea-js-insert-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org-mode

;;; org-mode options
(setq org-log-done 'time)
(setq org-directory my-workspace-path)
(setq org-export-html-validation-link nil)

;; ;;; Add an OPENED status to TODO stuff in org
;; ;;; (still buggy as hell)
;; (defcustom org-opened-string "OPENED:"
;;   "String used as the prefix for timestamps logging opening a TODO entry"
;;   :group 'org-keywords
;;   :type 'string)

;; (defun my-org-insert-opened ()
;;   "Insert an inactive timestamp with opened state to current element"
;;   (if (string= "TODO" org-state)
;;       (save-excursion
;; 	(org-back-to-heading)
;; 	(org-show-entry)
;; 	(end-of-line)
;; 	(if (looking-at "\\<OPENED: *\\[\\([^]]+\\)\\]")
;; 	    (let () (next-line 1)
;; 	    (kill-whole-line)
;; 	    (previous-line 1)
;; 	    (end-of-line)
;; 	    )
;; 	  )
;; 	(org-insert-time-stamp (current-time) t t "\n OPENED: ")
;; 	(indent-for-tab-command)
;; 	)
;;     )
;; )
;; (add-hook 'org-after-todo-state-change-hook 'my-org-insert-opened)

;;; Run auto-fill-mode when entering org mode
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
