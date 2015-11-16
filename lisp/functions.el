;;; functions.el --- Utility functions

;; Copyright (C) 2012  Michael Brennan

;; Author: Michael Brennan <brennan.brisad@gmail.com>
;; Keywords: lisp
;;; Commentary:

;; 

;;; Code:

(defun try-require (feature)
  "Try to load FEATURE.  On error just return nil."
  (condition-case err
      (require feature)
    (file-error nil)))

(defun set-window-width (width)
  "Set width of selected window to WIDTH."
  (interactive "p")
  (enlarge-window (- width (window-width)) t))

(defun set-80-columns ()
  "Set width of selected windows to 80 columns."
  (interactive)
  (set-window-width 80))

(defun upcase-last-word ()
  "Convert last word to upper case."
  (interactive)
  (upcase-word -1))

(defun my-insert-command-value (command)
  "Insert the return value of the COMMAND."
  (interactive "*C(insert) M-x ")
  (print (call-interactively command) (current-buffer)))

(defun insert-date ()
  "Insert the locale's \"preferred\" date format."
  (interactive)
  (insert (format-time-string "%x")))

(defun insert-time ()
  "Insert the locale's \"preferred\" time format."
  (interactive)
  (insert (format-time-string "%X")))

;; From http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file.  The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun find-files (dir args)
  "Run `find' and return its result as a list of strings.
DIR specifies the directory to search, ARGS specifices arguments to `find'"
  (let* ((command (concat "find " dir " " args " -print0"))
         (files (split-string (shell-command-to-string command) (char-to-string 0))))
    (remove-if-not '(lambda (x) (> (length x) 0)) files)))

(defun dbm-to-watt (dbm)
  "Show DBM in Watts."
  (interactive "nValue in dBm: ")
  (let ((watts (/ (expt 10 (/ dbm 10.0)) 1000)))
    (message (format "%.2f" watts))))

(defun watt-to-dbm (watt)
  "Show WATT in dBm."
  (interactive "nValue in W: ")
  (let ((dbm (+ 30 (* 10 (log watt 10)))))
    (message (format "%.2f" dbm))))

;;; functions.el ends here
