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

;;; functions.el ends here
