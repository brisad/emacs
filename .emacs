; -*- coding: utf-8 -*-
;
; Michael Brennan <brennan.brisad@gmail.com>
;

(add-to-list 'load-path "~/emacs")
; or (setq load-path (cons "~/.emacs/" load-path))
; or (push "~/.emacs/" load-path)

(load "functions")
(load "japanese-conf")
(load "org-conf")

(load-library "iso-transl")


;; Settings ;;

(prefer-coding-system 'utf-8)
(setq search-highlight t)
(setq query-replace-highlight t)
(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq transient-mark-mode nil)
(setq visible-bell t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq require-final-newline t)
(add-hook 'find-file-hook 'auto-insert)


;; Keybindings

; Map M-x to a more accessible key sequence
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key [f5] 'compile)
(global-set-key [f11] 'eshell)

(global-set-key (kbd "<f12>") 'menu-bar-mode)
(global-set-key (kbd "C-c o") 'join-line)

(global-set-key "\C-c-" 'set-80-columns)
(global-set-key "\C-cu" 'upcase-last-word)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c p") 'proced)

(global-set-key (kbd "C-c i d") 'insert-date)
(global-set-key (kbd "C-c i t") 'insert-time)


;; calendar ;;
(setq calendar-week-start-day 1)


;; imenu ;;

;; Start imenu if possible
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)


;; Winner Mode ;;

;; Start Winner Mode
(when (fboundp 'winner-mode)
      (winner-mode 1))
(windmove-default-keybindings)


;; iswitchb ;;
(require 'iswitchb)
(iswitchb-mode 1)

;; column-marker ;;
(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))


;; Colors ;;

(set-face-background 'region "gray95")
(set-face-foreground 'default "black")
(set-face-foreground 'region "gray60")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
