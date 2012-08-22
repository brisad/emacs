;;; org-conf.el --- org-mode configuration

;; Copyright (C) 2012  Michael Brennan

;; Author: Michael Brennan <brennan.brisad@gmail.com>
;; Keywords: 

;;; Commentary:

;; 

;;; Code:

(setq load-path (cons "~/emacs/org/lisp" load-path))
(setq load-path (cons "~/emacs/org/contrib/lisp" load-path))

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))

(setq org-agenda-files '("~/org"))
(if (eq system-type 'windows-nt)
    (setq org-agenda-files (cons "E:/" org-agenda-files)))

(setq org-agenda-start-on-weekday nil)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

(setq org-default-notes-file "E:/notes.org")
(define-key global-map "\C-cc" 'org-capture)

;;; org-conf.el ends here
