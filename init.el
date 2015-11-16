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
        '(python-mode magit yasnippet jedi auto-complete
                      find-file-in-repository flycheck
                      smartparens clojure-mode cider
                      flx-ido smex expand-region projectile ag))

  (if (cl-notevery 'package-installed-p to-install)
      (if (y-or-n-p "Some packages are missing. Download them?")
          (progn
            ;; Now that we've got a yes we can do network activity and
            ;; refresh contents before performing the downloads.
            (package-refresh-contents)
            (mapc 'install-if-needed to-install)))))

(add-to-list 'load-path "~/.emacs.d/lisp")
; or (setq load-path (cons "~/.emacs/" load-path))
; or (push "~/.emacs/" load-path)

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
(setq mouse-yank-at-point t)
(setq apropos-do-all t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq load-prefer-newer t)

;; Backup settings
(setq version-control t  ; Put version numbers on backup files
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      kept-old-versions 2
      kept-new-versions 9
      delete-old-versions t)


;; Keybindings

(global-set-key (kbd "M-_") 'hippie-expand)

; Map M-x to a more accessible key sequence, overridden if smex is
; available below
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

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; expand-region ;;
(if (try-require 'expand-region)
    (global-set-key (kbd "C-'") 'er/expand-region))

;; Magit ;;
(if (try-require 'magit)
    (global-set-key "\C-xg" 'magit-status))

;; calendar ;;
(setq calendar-week-start-day 1)


;; Projectile ;;
(projectile-global-mode)

;; Ag ;;
(setq ag-highlight-search t)

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


;; Save Place
(require 'saveplace)
(setq-default save-place t)

;; autocomplete ;;
(when (try-require 'auto-complete)
  (ac-config-default)
  (setq
   ac-use-menu-map t
   ac-candidate-limit 20)
  (define-key ac-mode-map (kbd "<C-tab>") 'auto-complete))


(when (try-require 'yasnippet)
  ;(yas-global-mode 1)
  (yas-reload-all)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'c++-mode-hook 'yas-minor-mode))

;; Jedi ;;
(if (try-require 'jedi)
    (add-hook 'python-mode-hook
              (lambda ()
                (jedi:setup)
                (local-set-key "\C-cd" 'jedi:show-doc)
                (setq jedi:use-shortcuts t))))

;; flycheck ;;
(if (try-require 'flycheck)
    (global-flycheck-mode t))

;; ido-mode ;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(ido-mode t)

;; flx-ido ;;
(when (try-require 'flx-ido)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

;; smex ;;
(if (try-require 'smex)
    (progn
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)
      (global-set-key (kbd "C-x C-m") 'smex)
      (global-set-key (kbd "C-c C-m") 'smex)
      ; The old M-x.
      (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

;; column-marker ;;
(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Colors ;;

(set-face-background 'region "gray95")
(set-face-foreground 'default "black")
(set-face-foreground 'region "gray60")

; Don't disable narrow-to-region
(put 'narrow-to-region 'disabled nil)

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