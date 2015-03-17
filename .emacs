; -*- coding: utf-8 -*-
;
; Michael Brennan <brennan.brisad@gmail.com>
;

(when (>= emacs-major-version 24)
  (require 'cl)
  (require 'package)
  (package-initialize)

  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (defun install-if-needed (package)
    (unless (package-installed-p package)
      (package-install package)))

  ;; make more packages available with the package installer
  (setq to-install
        '(python-mode magit yasnippet jedi auto-complete autopair
                      find-file-in-repository flycheck
                      smartparens))

  (if (cl-notevery 'package-installed-p to-install)
      (if (y-or-n-p "Some packages are missing. Download them?")
          (progn
            ;; Now that we've got a yes we can do network activity and
            ;; refresh contents before performing the downloads.
            (package-refresh-contents)
            (mapc 'install-if-needed to-install)))))

(add-to-list 'load-path "~/emacs")
; or (setq load-path (cons "~/.emacs/" load-path))
; or (push "~/.emacs/" load-path)
(add-to-list 'load-path "~/emacs/yaml-mode")

(load "functions")
(load "japanese-conf")
(load "org-conf")

(load-library "iso-transl")

(package-initialize)

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
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq require-final-newline t)
(add-hook 'find-file-hook 'auto-insert)


;; Keybindings

; Map M-x to a more accessible key sequence
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key [f5] 'compile)
(global-set-key [f7] 'find-file-in-repository)
(global-set-key [f9] 'eshell-here)
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

;; Magit ;;
(if (try-require 'magit)
    (global-set-key "\C-xg" 'magit-status))

;; calendar ;;
(setq calendar-week-start-day 1)


;; Smartparens mode ;;

(when (try-require 'smartparens-config)
  (smartparens-global-mode t)
  (setq sp-base-key-bindings 'paredit)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode t))

;; Python mode ;;
(when (try-require 'python-mode)
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  (setq py-electric-colon-active t))
;;I want Ctrl-BKSP to work and else: jumps to left

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


;; autocomplete ;;
(when (try-require 'auto-complete)
  (setq
   ac-use-menu-map t
   ac-candidate-limit 20)
  (add-hook 'python-mode-hook 'auto-complete-mode)
  (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode))

(if (try-require 'autopair)
    (add-hook 'python-mode-hook 'autopair-mode))

(when (try-require 'yasnippet)
  ;(yas-global-mode 1)
  (yas-reload-all)
  (add-hook 'python-mode-hook 'yas-minor-mode))

;; Jedi ;;
(if (try-require 'jedi)
    (add-hook 'python-mode-hook
              (lambda ()
                (jedi:setup)
                (local-set-key "\C-cd" 'jedi:show-doc)
                (local-set-key (kbd "M-.") 'jedi:goto-definition))))

;; flycheck ;;
(if (try-require 'flycheck)
    (global-flycheck-mode t))

;; ido-mode ;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(ido-mode t)

;; column-marker ;;
(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; yaml-mode ;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; Colors ;;

(set-face-background 'region "gray95")
(set-face-foreground 'default "black")
(set-face-foreground 'region "gray60")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.4)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
