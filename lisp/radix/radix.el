;;; radix.el --- Change radix of number at point     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Michael Hoffmann

;; Author: Michael Hoffmann <brennan.brisad@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a keybinding "C-c C-z" which cycles the number at point
;; between the bases 10, 16 and 2.

;;; Code:


(provide 'radix)
(require 'ert)
(require 'calc-bin)

(defun radix--change-base (num base)
  "Return string of NUM represented in BASE."
  (let ((calc-number-radix base))
    (math-format-radix num)))

(defun radix--parse (s)
  "Parse S and return its value and base as a cons cell."
  (let* ((base (cond ((string-match-p "^0x" s) 16)
                     ((string-match-p "^0b" s) 2)
                     (t 10)))
         (toparse (if (= base 10) s (substring s 2))))
    (cons (string-to-number toparse base) base)))

(defun replace-bounds-with (bounds str)
  "In the current buffer, replace whatever is in BOUNDS with STR."
  (delete-region (car bounds) (cdr bounds))
  (insert str))

(defun radix--cycle-radix (val)
  "Cycle radix of VAL."
  (let* ((parsed (radix--parse val))
         (val (car parsed))
         (base (cdr parsed)))
    (cond ((= base 2) (radix--change-base val 10))
          ((= base 16) (concat "0b" (radix--change-base val 2)))
          (t (concat "0x" (radix--change-base val 16))))))

(defun cycle-radix-at-point ()
  "Cycle radix of the number at point."
  (interactive)
  (replace-bounds-with
   (bounds-of-thing-at-point 'symbol)
   (radix--cycle-radix (thing-at-point 'symbol))))

(global-set-key (kbd "C-c C-z") 'cycle-radix-at-point)

(ert-deftest radix--change-base ()
  (should (string-equal (radix--change-base 10 16) "A"))
  (should (string-equal (radix--change-base #2r1010 2) "1010")))

(ert-deftest radix--parse ()
  (should (equal (radix--parse "0x100") '(256 . 16)))
  (should (equal (radix--parse "0b100") '(4 . 2)))
  (should (equal (radix--parse "100") '(100 . 10))))

(ert-deftest radix--cycle-radix ()
  (should (string-equal (radix--cycle-radix "65") "0x41"))
  (should (string-equal (radix--cycle-radix "0xf0") "0b11110000"))
  (should (string-equal (radix--cycle-radix "0xF0") "0b11110000"))
  (should (string-equal (radix--cycle-radix "0b0101") "5")))

;;; radix.el ends here
