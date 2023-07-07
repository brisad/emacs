; -*- coding: utf-8 -*-
;
; Michael Hoffmann <brennan.brisad@gmail.com>
;

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp")
; or (setq load-path (cons "~/.emacs/" load-path))
; or (push "~/.emacs/" load-path)

(defun visit-init-dot-el ()
  "Visit init.el file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "C-c d") 'visit-init-dot-el)

(load "functions")
(load "japanese-conf")
(load "radix/radix")

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
(setq set-mark-command-repeat-pop t)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq require-final-newline t)
(add-hook 'find-file-hook 'auto-insert)
(setq mouse-yank-at-point t)
(setq apropos-do-all t)
(setq load-prefer-newer t)
(setq-default frame-title-format "%b (%f)")
(setq initial-scratch-message nil)
(setq show-paren-style 'expression)
(setq show-paren-delay 0)
(show-paren-mode t)
(global-prettify-symbols-mode 1)
(save-place-mode t)
(setq dired-dwim-target t)

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(use-package counsel)

(use-package ivy
  :after (counsel)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  (("C-s" . swiper)
   ("C-c s" . isearch-forward-regexp)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> x" . counsel-find-library)
   ("<f2> u" . counsel-unicode-char)))

;; Backup settings
(setq version-control t  ; Put version numbers on backup files
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      kept-old-versions 2
      kept-new-versions 9
      delete-old-versions t)


;; Keybindings
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "M-_") 'hippie-expand)

(global-set-key [f5] 'compile)
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

(global-set-key (kbd "<f8>") #'mh/toggle-check-writing)

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "s-B") 'ibuffer)
(global-set-key (kbd "s-b") 'ido-switch-buffer)

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind ("<f6>" . hs-toggle-hiding))

;; Jumping to visible text
(use-package avy
  :bind (("M-p" . avy-goto-subword-1)
         (:map isearch-mode-map ("C-'" . avy-isearch)))
  :config
  (setq avy-background t))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/mark-next-symbol-like-this)
         ("C-M-<" . mc/mark-previous-symbol-like-this)
         ("C-S-SPC" . set-rectangular-region-anchor)))

(use-package wgrep)

(use-package expand-region
  :bind ("C-'" . er/expand-region))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package find-file-in-repository
  :bind ([f7] . find-file-in-repository))

;; calendar ;;
(setq calendar-week-start-day 1)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ag
  :config
  (setq ag-highlight-search t))


;; Org-mode
(use-package org
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-log-done 'time)
  (setq org-agenda-files (list org-directory))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t) (python . t)))
(setq org-confirm-babel-evaluate nil)

;; Paredit ;;
(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (clojure-mode . enable-paredit-mode)))

;; imenu ;;

;; Start imenu if possible
(defun try-to-add-imenu ()
  (condition-case
      nil
      (progn
        (imenu-add-to-menubar "Imenu")
        (setq imenu-auto-rescan t))
    (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)


;; Winner Mode ;;

;; Start Winner Mode
(when (fboundp 'winner-mode)
      (winner-mode 1))
(windmove-default-keybindings)

;; Jedi ;;
(use-package company-jedi
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq jedi:use-shortcuts t)
              (jedi:setup)
              (local-set-key "\C-cd" 'jedi:show-doc)
              (company-mode)
              (add-to-list 'company-backends 'company-jedi))))


(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Interactively do things
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-create-new-buffer 'always)
  (ido-mode 1)
  (ido-everywhere))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :after ido
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (setq ido-vertical-show-count t))

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

;; M-x enhancement
(use-package smex
  :config
  (smex-initialize)
  :bind (("M-X" . smex-major-mode-commands)
         ("C-c C-m" . smex)
         ;; The old M-x.
         ("C-c C-c M-x" . execute-extended-command)))

;; imenu tag navigation across buffers
(use-package imenu-anywhere
  :bind ("C-." . imenu-anywhere))

;; column-marker ;;
(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Show minibuffer documentation when editing lisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)

;; JS
(use-package flow-minor-mode)

(defconst js-modes
  '(js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode web-mode))

(dolist (mode js-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook
              (lambda ()
                (flow-minor-enable-automatically)
                (company-mode)
                (setq company-flow-executable (expand-file-name "flow" (node-modules-bindir)))))))

(use-package company-flow
  :after company
  :config
  (add-to-list 'company-backends 'company-flow))

(defun checkers-for-mode (mode)
  "Return list of flycheck checkers configured for MODE."
  (seq-remove
   (lambda (checker)
     (not (flycheck-checker-supports-major-mode-p checker mode)))
   flycheck-checkers))

(defun javascript-checkers ()
  "Return list of configured javascript flycheck checkers."
  (delete-dups
   (apply
    #'append
    (mapcar #'checkers-for-mode js-modes))))

(defun node-modules-bindir ()
  "Return bin directory for node_modules in scope."
  (let ((root (locate-dominating-file (or (buffer-file-name)
                                          default-directory)
                                      "node_modules")))
    (when root
      (expand-file-name "node_modules/.bin/" root))))

(defun use-checkers-from-node-modules ()
  "Setup JS flycheck checkers to use binaries in node_modules."
  (let ((bindir (node-modules-bindir)))
    (dolist (checker (javascript-checkers))
      (let* ((executable (flycheck-checker-executable checker))
             (executable-variable (flycheck-checker-executable-variable checker)))
        (when (not (symbol-value executable-variable))
          (let ((new-executable (expand-file-name executable bindir)))
            (when (file-executable-p new-executable)
              (make-local-variable executable-variable)
              (set executable-variable new-executable))))))))

(add-hook 'flycheck-mode-hook #'use-checkers-from-node-modules)

(use-package web-mode
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  :hook
  (web-mode . (lambda ()
                (if (equal web-mode-content-type "javascript")
                    (web-mode-set-content-type "jsx")))))

(use-package flycheck-flow
  :after flycheck
  :config
  (flycheck-add-mode 'javascript-flow 'web-mode)
  (flycheck-add-mode 'javascript-flow-coverage 'web-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
  (flycheck-add-next-checker 'javascript-flow-coverage 'javascript-eslint))

;; Rust
(use-package rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :hook (rust-mode . racer-mode)
  :bind
  (:map rust-mode-map
        ("TAB" . company-indent-or-complete-common)
        ("C-c d" . racer-describe)
        ("C-c C-d" . racer-describe)))

(use-package company
  :hook (racer-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;; Clojure
(use-package clojure-mode)
(use-package cider)

;; Colors ;;

(set-face-background 'region "burlywood")
(set-face-foreground 'default "black")
(set-face-foreground 'region "gray60")

; Don't disable narrow-to-region
(put 'narrow-to-region 'disabled nil)

(defun mh/toggle-check-writing ()
  "Toggle spelling and whitespace modes."
  (interactive)
  (if (and (bound-and-true-p flyspell-mode)
           (bound-and-true-p whitespace-mode))
      (progn
        (flyspell-mode -1)
        (whitespace-mode -1))
    (if (derived-mode-p 'prog-mode)
        (flyspell-prog-mode)
      (flyspell-mode 1))
    (flyspell-buffer)
    (whitespace-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("dired"
       (saved . "dired"))
      ("perf"
       (saved . "perf"))
      ("ag"
       (saved . "ag"))))))
 '(ibuffer-saved-filters
   (quote
    (("emacs"
      ((or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*Help\\*$"))))
     ("dired"
      ((used-mode . dired-mode)))
     ("perf"
      ((filename . "perf.html")))
     ("magit"
      ((used-mode . magit-status-mode)))
     ("ag"
      ((used-mode . ag-mode)))
     ("programming"
      ((or
        (mode . python-mode)
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (racer cargo rust-mode company-flow flx-ido company-jedi magit expand-region wgrep web-mode use-package swiper smex projectile paredit js2-mode iy-go-to-char imenu-anywhere iedit ido-vertical-mode ido-completing-read+ highlight-thing flycheck-rust flycheck-flow flow-minor-mode find-file-in-repository cider avy ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
