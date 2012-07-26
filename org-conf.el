;;; org-conf.el --- org-mode configuration

;; Copyright (C) 2012  Michael Brennan

;; Author: Michael Brennan <brennan.brisad@gmail.com>
;; Keywords: 

;;; Commentary:

;; 

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))

;(setq org-default-notes-file (concat org-directory "/notes.org"))
;(define-key global-map "\C-cc" 'org-capture)

;;; org-conf.el ends here
