;;; org-mode options
(setq org-log-done 'time)
(setq org-directory "C:\\Users\\ANDREA\\Dropbox\\org")
(setq org-export-html-validation-link nil)

;;; miscellaneous options
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)

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
(global-set-key [f1] 'shell)


;;; add folder from where load custom lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

;;; Set default package repositories
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;;; Start Evil mode by default
; (package-initialize)
; (evil-mode 0)

;;; Needed to be able to insert ascii characters in decimal with C-q
(setq read-quoted-char-radix 10)

;;; Load default directory from dropbox
(setq default-directory "C:\\Users\\ANDREA\\Dropbox\\org")
(cd "C:\\Users\\ANDREA\\Dropbox\\org")

;;; Set to open the meditation file on startup
(setq initial-buffer-choice "C:\\Users\\ANDREA\\Dropbox\\org\\meditazioni.org")
(setq initial-scratch-message "")

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

;;; save all backups in one folder
(setq backup-directory-alist '(("."."~/.emacs.d/saves")))

;;; Custom Keybindings
(global-set-key (kbd "C-x C-j") 'emmet-expand-line)

;;; Auto refresh directories in dired mode
(add-hook 'dired-mode-hook 'auto-revert-mode)
