; -*- coding: utf-8 -*-
;
; Michael Hoffmann <brennan.brisad@gmail.com>
;

(require 'cl)
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(use-package magit company-jedi iedit
              virtualenvwrapper
              find-file-in-repository flycheck
              paredit clojure-mode cider
              ido-completing-read+ flx-ido smex
              ido-vertical-mode imenu-anywhere
              expand-region projectile ag
              rust-mode cargo flycheck-rust racer
              avy flycheck-flow iy-go-to-char
              flow-minor-mode company-flow
              web-mode
              ))

(if (cl-notevery 'package-installed-p to-install)
    (if (y-or-n-p "Some packages are missing. Download them?")
        (progn
          ;; Now that we've got a yes we can do network activity and
          ;; refresh contents before performing the downloads.
          (package-refresh-contents)
          (mapc 'install-if-needed to-install))))

(eval-when-compile (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/lisp")
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
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq require-final-newline t)
(add-hook 'find-file-hook 'auto-insert)
(setq mouse-yank-at-point t)
(setq apropos-do-all t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq load-prefer-newer t)
(setq-default frame-title-format "%b (%f)")
(setq initial-scratch-message nil)
(show-paren-mode t)
(setq show-paren-style 'expression)

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

;; avy and iy
(global-set-key (kbd "M-p") 'avy-goto-char)
(global-set-key (kbd "M-n") 'iy-go-to-char)

(use-package iedit)

(use-package expand-region
  :bind ("C-'" . er/expand-region))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

;; calendar ;;
(setq calendar-week-start-day 1)


;; Projectile ;;
(projectile-global-mode)

;; Ag ;;
(setq ag-highlight-search t)

;; Paredit ;;
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

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


;; Save Place
(require 'saveplace)
(setq-default save-place t)


(defun setup-jedi-company-mode ()
  (company-mode)
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'setup-jedi-company-mode)

;; Jedi ;;
(add-hook 'python-mode-hook
          (lambda ()
            (setq jedi:use-shortcuts t)
            (jedi:setup)
            (local-set-key "\C-cd" 'jedi:show-doc)))

;; virtualenvwrapper
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs/")

(use-package flycheck
  :init (global-flycheck-mode))

;; ido-mode ;;
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-create-new-buffer 'always)
(ido-mode 1)
(ido-everywhere)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

;; smex ;;
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
;; The old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; imenu-anywhere ;;
(global-set-key (kbd "C-.") #'imenu-anywhere)

;; column-marker ;;
(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Show minibuffer documentation when editing lisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)

;; JS
(defconst js-modes
  '(js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode web-mode))

(dolist (mode js-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook
              (lambda ()
                (flow-minor-enable-automatically)
                (company-mode)
                (setq company-flow-executable (expand-file-name "flow" (node-modules-bindir)))))))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-flow))

(defun checkers-for-mode (mode)
  "Return list of flycheck checkers configured for MODE."
  (remove-if
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

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (if (equal web-mode-content-type "javascript")
                (web-mode-set-content-type "jsx"))))

(require 'flycheck-flow)
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-flow 'web-mode)
  (flycheck-add-mode 'javascript-flow-coverage 'web-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
  (flycheck-add-next-checker 'javascript-flow-coverage 'javascript-eslint))

;; Rust
(require 'rust-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(define-key rust-mode-map (kbd "C-c d") #'racer-describe)
(define-key rust-mode-map (kbd "C-c C-d") #'racer-describe)
(setq company-tooltip-align-annotations t)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Colors ;;

(set-face-background 'region "burlywood")
(set-face-foreground 'default "black")
(set-face-foreground 'region "gray60")

; Don't disable narrow-to-region
(put 'narrow-to-region 'disabled nil)

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
 '(js-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
