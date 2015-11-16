;;; japanese-conf.el --- Japanese in emacs

;; Copyright (C) 2012  Michael Brennan

;; Author: Michael Brennan <brennan.brisad@gmail.com>
;; Keywords: i18n, faces

;;; Commentary:

;;

;;; Code:

;;
;; Japanese input configuration to go here
;;

;; Fix Japanese font
(cond
 (window-system
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("VL Gothic" . "unicode-bmp"))
 )
)

;;; japanese.el ends here
