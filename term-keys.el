;;; term-keys.el --- Lossless keyboard input in a terminal emulator

;; Version: 0.1.0
;; Author: Vladimir Panteleev
;; Url: https://github.com/CyberShadow/term-keys
;; Keywords: input
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package allows lossless keyboard input when using Emacs from a
;; terminal emulator.

;; For more information, please see the accompanying README.md file.

;;; Code:

(defvar term-keys/mapping)
(setq term-keys/mapping
      '(("Escape" . "<escape>")
	("F1" . "<f1>")
	("F2" . "<f2>")
	("F3" . "<f3>")
	("F4" . "<f4>")
	("F5" . "<f5>")
	("F6" . "<f6>")
	("F7" . "<f7>")
	("F8" . "<f8>")
	("F9" . "<f9>")
	("F10" . "<f10>")
	("F11" . "<f11>")
	("F12" . "<f12>")
	("Print" . "<print>")
	("Scroll_Lock" . "<Scroll_Lock>")
	("Pause" . "<pause>")

	("grave" . "`")
	("1" . "1")
	("2" . "2")
	("3" . "3")
	("4" . "4")
	("5" . "5")
	("6" . "6")
	("7" . "7")
	("8" . "8")
	("9" . "9")
	("0" . "0")
	("minus" . "-")
	("equal" . "=")
	("BackSpace" . "<backspace>")
	("Tab" . "<tab>")
	("q" . "q")
	("w" . "w")
	("e" . "e")
	("r" . "r")
	("t" . "t")
	("y" . "y")
	("u" . "u")
	("i" . "i")
	("o" . "o")
	("p" . "p")
	("bracketleft" . "[")
	("bracketright" . "]")
	("backslash" . "\\")
	("Caps_Lock" . "<Caps_Lock>")
	("a" . "a")
	("s" . "s")
	("d" . "d")
	("f" . "f")
	("g" . "g")
	("h" . "h")
	("j" . "j")
	("k" . "k")
	("l" . "l")
	("semicolon" . ";")
	("apostrophe" . "'")
	("Return" . "<return>")
	("Shift_L" . nil)
	("z" . "z")
	("x" . "x")
	("c" . "c")
	("v" . "v")
	("b" . "b")
	("n" . "n")
	("m" . "m")
	("comma" . ",")
	("period" . ".")
	("slash" . "/")
	("Shift_R" . nil)
	("Ctrl_L" . nil)
	("Super_L" . nil)
	("Alt_L" . nil)
	("space" . "SPC")
	("Alt_R" . nil)
	("Super_R" . nil)
	("Menu" . "<menu>")
	("Ctrl_R" . nil)

	("Up" . "<up>")
	("Down" . "<down>")
	("Left" . "<left>")
	("Right" . "<right>")

	("Insert" . "<insert>")
	("Delete" . "<delete>")
	("Home" . "<home>")
	("End" . "<end>")
	("Prior" . "<prior>")
	("Next" . "<next>")

	;; Add new entries at the end of the list, to avoid disrupting
	;; existing configurations.

	;; TODO: numpad
	))

(defvar term-keys/prefix)
(setq term-keys/prefix "\033\037")
(defvar term-keys/suffix)
(setq term-keys/suffix "}")

;; TODO:
;; Arrow keys - C-S-, C-M-, C-M-S-
;; PgUp/Dn - C-S-, M-, C-M-, S-(?)
;; C-M-g
;; C-TAB, C-S-TAB

(defun term-keys/want-key-p (key shift control alt)
  "."
  (or
   (and (string-equal key "g") control alt)
   (and (member key '("Up" "Down" "Left" "Right" "Home" "End" "Prior" "Next")) control (or shift alt))
   (and (string-equal key "Tab") control)
   ))

(defun term-keys/format-key (key shift control alt)
  "."
  (concat
   (if shift "S-" "")
   (if control "C-" "")
   (if alt "M-" "")
   key))

(defun term-keys/encode-key (key shift control alt)
  "."
  (format "%x%x"
	  (+
	   (if shift 1 0)
	   (if control 2 0)
	   (if alt 4 0))
	  key))

(require 'cl-lib)
(defun term-keys/init ()
  "."
  ;; (global-unset-key (kbd "M-{"))

  (let ((num 0)
  	(keys term-keys/mapping))
    (while keys
      (let ((pair (car keys)))
	(cl-loop
	 for shift in '(nil t) do
	 (cl-loop
	  for control in '(nil t) do
	  (cl-loop
	   for alt in '(nil t)
	   if (and (cdr pair) (term-keys/want-key-p (car pair) shift control alt))
	   do (define-key
		input-decode-map
		(concat
		 term-keys/prefix
		 (term-keys/encode-key num shift control alt)
		 term-keys/suffix)
		(kbd (term-keys/format-key (cdr pair) shift control alt)))))))
      (setq num (1+ num))
      (setq keys (cdr keys))))
  (message "term-keys/init !!!")
  )

(defun term-keys/urxvt-args ()
  "!"

  (let ((num 0)
	(keys term-keys/mapping)
	(args))
    (while keys
      (let ((pair (car keys)))
	(cl-loop
	 for shift in '(nil t) do
	 (cl-loop
	  for control in '(nil t) do
	  (cl-loop
	   for alt in '(nil t)
	   if (and (cdr pair) (term-keys/want-key-p (car pair) shift control alt))
	   do (setq args
		    (apply #'list
			   (concat
			    "-keysym."
			    (term-keys/format-key (car pair) shift control alt))
			   (concat
			    "string:"
			    term-keys/prefix
			    (term-keys/encode-key num shift control alt)
			    term-keys/suffix)
			   args))))))
      (setq num (1+ num))
      (setq keys (cdr keys)))
    args))

(defun term-keys/test ()
  "Test it!"
  (apply #'call-process "/bin/urxvt" nil nil nil
	 (append
	  (term-keys/urxvt-args)
	  '(
	    "-e" "/bin/emacs" "-nw"
	    "--load" "/home/vladimir/tmp/2017-11-04-scratch/23:38:39/term-keys.el"
	    "-q"
	    "--funcall" "term-keys/init"
	    ))))

(when (boundp 'term-keys/devel)
  (term-keys/test))

(provide 'term-keys)
;;; term-keys.el ends here
