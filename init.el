(package-initialize)


;;; miscellaneous options
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p) 
(set-fringe-mode '(0 . 1))
(follow-mode 1)

;;; Remove toolbar and menu
(menu-bar-mode 0)
(tool-bar-mode 0)

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
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))

;;; Start Evil mode by default
; (package-initialize)
; (evil-mode 0)

;;; Needed to be able to insert ascii characters in decimal with C-q
(setq read-quoted-char-radix 10)

;;; Load default directory from dropbox
(setq default-directory "C:\\Users\\ANDREA\\Dropbox\\org")
(cd "C:\\Users\\ANDREA\\Dropbox\\org")

;;; Set to open the meditation file on startup
(setq initial-buffer-choice 
      "C:\\Users\\ANDREA\\Dropbox\\org\\meditazioni.org")
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
(load "calendarpage.el")

;;; save all backups in one folder
(setq backup-directory-alist '(("."."~/.emacs.d/saves")))

;;; Custom Keybindings
(global-set-key (kbd "C-x C-j") 'emmet-expand-line)

;;; Auto refresh directories in dired mode
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; inrease/decrease font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;; Search and replace, no query
(global-set-key (kbd "C-%") 'replace-string)


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
    (insert "- Luna :: ")(newline)
    (insert "- Cond. Fisiche :: ")(newline)
    (insert "- Cond. Mentali :: ")(newline)
    (insert "- 5 Riti Tibetani :: ")(newline)
    (insert "- LBRP :: ")(newline)
    (insert "- Middle Pillar :: ")(newline)
    (insert "- Kun lun :: ")
    (goto-char my-point)
    )
  )

(defun andrea-add-journal-entry ()
  "Insert journaling entry for meditation at the end of the current file."
  (interactive)
  (goto-char (point-max))
  (insert "* ")
  (andrea-insert-timestamp)(newline))

(defun andrea-open-journal ()
  "Automatically save current buffer, open journal and add new entry"
  (interactive)
  (let ()
    (save-buffer)
    (find-file "c:/Users/ANDREA/Dropbox/org/giornale.org")
    (andrea-add-journal-entry)
  )
)

(global-set-key (kbd "C-c M-m") 'andrea-add-meditation-entry)
(global-set-key (kbd "C-c M-j") 'andrea-add-journal-entry)
(global-set-key (kbd "C-c M-n") 'andrea-open-journal)

;;; misc shortcuts
(global-set-key (kbd "<f8>") 'calc)
(global-set-key (kbd "<f1>") 'shell)
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
  (find-file "~\\.emacs.d\\init.el"))

;;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Automatically load follow-mode
(follow-mode 1)

;;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-M-'") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File editing
(defun andrea-move-line-above ()
  "Moves current line above the upper one"
  (interactive)
  (let ((point-from-start (- (point) (line-beginning-position))))
    (kill-whole-line)
    (ignore-errors (previous-line))
    (move-beginning-of-line nil)
    (yank)
    (previous-line)
    (goto-char (+ (line-beginning-position) point-from-start))))

(defun andrea-move-line-below ()
  "Moves current line below the lower one"
  (interactive)
  (let ((point-from-start (- (point) (line-beginning-position))))
    (kill-whole-line)
    (ignore-errors (next-line))
    (move-beginning-of-line nil)
    (yank)
    (previous-line)
    (goto-char (+ (line-beginning-position) point-from-start))))

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
(defun andrea-indent-brackets ()
  "Automatically in"
  (interactive)
  (progn
    (backward-char)
    (newline)
    (newline-and-indent)
    (previous-line)
    (indent-for-tab-command)))

(global-set-key (kbd "C-.") 'andrea-indent-brackets)

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
(setq org-directory "C:\\Users\\ANDREA\\Dropbox\\org")
(setq org-export-html-validation-link nil)

;;; Add an OPENED status to TODO stuff in org
;;; (still buggy as hell)
(defcustom org-opened-string "OPENED:"
  "String used as the prefix for timestamps logging opening a TODO entry"
  :group 'org-keywords
  :type 'string)

(defun my-org-insert-opened ()
  "Insert an inactive timestamp with opened state to current element"
  (if (string= "TODO" org-state)
      (save-excursion
	(org-back-to-heading)
	(org-show-entry)
	(end-of-line)
	(if (looking-at "\\<OPENED: *\\[\\([^]]+\\)\\]")
	    (let () (next-line 1)
	    (kill-whole-line)
	    (previous-line 1)
	    (end-of-line)
	    )
	  )
	(org-insert-time-stamp (current-time) t t "\n OPENED: ")
	(indent-for-tab-command)
	)
    )
)
(add-hook 'org-after-todo-state-change-hook 'my-org-insert-opened)

;;; Run auto-fill-mode when entering org mode
(add-hook 'org-mode-hook 'auto-fill-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
