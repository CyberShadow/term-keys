;;; term-keys-alacritty.el --- term-keys support for Alacritty

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

;; This file contains supplementary code for aiding in the
;; configuration of the Alacritty terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)


(defgroup term-keys/alacritty nil
  "`term-keys' options for ALACRITTY-based terminal emulators."
  :group 'term-keys)

(define-widget 'term-keys/alacritty-modifier 'lazy
  "Choice for Alacritty key modifier state flags."
  :type '(choice (const "Command")
		 (const "Control")
		 (const "Option")
		 (const "Super")
		 (const "Shift")
		 (const "Alt")
		 (const :tag "(none)" nil)))


(defcustom term-keys/alacritty-modifier-map ["Shift" "Control" "Alt" "Super" "Command" "Option"]
  "Map of Alacritty modifiers to Emacs modifiers.

This should be a vector of 6 elements, with each element being a
string indicating the name of the Alacritty modifier name
corresponding to the Emacs modifiers Shift, Control, Meta, Super,
Hyper and Alt respectively.  nil indicates that there is no
mapping for this modifier."
  :type '(vector
	  (term-keys/alacritty-modifier :tag "Shift")
	  (term-keys/alacritty-modifier :tag "Control")
	  (term-keys/alacritty-modifier :tag "Meta")
	  (term-keys/alacritty-modifier :tag "Super")
	  (term-keys/alacritty-modifier :tag "Hyper")
	  (term-keys/alacritty-modifier :tag "Alt"))
  :group 'term-keys/alacritty)


(defun term-keys/alacritty-mods-representable (mods)
  "Return non-nil if the given MODS vector is representable in ALACRITTY."
  (cl-reduce (lambda (x y) (and x y)) ; all
	     (mapcar (lambda (n)
		       (or (not (elt mods n)) ; inactive modifier
			   (elt term-keys/alacritty-modifier-map n))) ; mapped
		     (number-sequence 0 (1- (length mods)))))) ; 0..5


(defun term-keys/alacritty-format-mods (mods)
  "Format MODS in Alacritty syntax."
  (if (cl-reduce (lambda (x y) (or x y)) mods)
      (concat
       ", mods: "
       (mapconcat
	(lambda (n)
	  (elt term-keys/alacritty-modifier-map n))
	(cl-remove-if-not (lambda (n) (elt mods n))
			  (number-sequence 0 (1- (length mods))))
	"|"))
    ""))


(defun term-keys/alacritty-config ()
  "Construct Alacritty configuration (alacritty.yml fragment).

This function returns, as a string, an alacritty.yml fragment
necessary to configure Alacritty to encode term-keys key
sequences, according to the term-keys configuration."
  (apply #'concat
	 "key_bindings:\n"
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (format "- { key: %s%s, chars: \"%s\" }\n"
		    (elt keymap 8)
		    (term-keys/alacritty-format-mods mods)
		    (mapconcat
		     (lambda (c) (format "\\x%02x" c))
		     (append
		      term-keys/prefix
		      (term-keys/encode-key index mods)
		      term-keys/suffix
		      nil)
		     ""))))))


(provide 'term-keys-alacritty)
;;; term-keys-alacritty.el ends here
