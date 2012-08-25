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

;; Usb stick is used to store org files that are used between
;; different computers, windows at work and linux at home.
(let ((usb-stick
       (cond ((eq system-type 'gnu/linux) "/media/E/")
             ((eq system-type 'windows-nt) "E:/"))))
  (setq org-agenda-files (cons usb-stick org-agenda-files))
  (setq org-default-notes-file (concat usb-stick "notes.org")))

(setq org-agenda-start-on-weekday nil)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

;; This assumes Org version greater than 6.36
(define-key global-map "\C-cc" 'org-capture)

;;; org-conf.el ends here
