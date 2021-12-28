;;; term-keys-x11.el --- term-keys X11 support code

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

;; This file contains supporting code for aiding in the configuration
;; of X11 terminal emulators, such as st and urxvt, to interoperate
;; with the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:

(require 'term-keys)

(defgroup term-keys/x11 nil
  "`term-keys' options for X11-based terminal emulators."
  :group 'term-keys)


(define-widget 'term-keys/x11-modifier 'lazy
  "Choice for X11 key modifier state flags."
  :type '(choice (const "Shift")
		 (const "Lock")
		 (const "Control")
		 (const "Mod1" :tag "Mod1 (usually Alt)")
		 (const "Mod2" :tag "Mod2 (usually Num Lock)")
		 (const "Mod3")
		 (const "Mod4" :tag "Mod4 (usually the Windows Logo key)")
		 (const "Mod5")
		 (const :tag "(none)" nil)))


(defcustom term-keys/x11-modifier-map ["Shift" "Control" "Mod1" "Mod4" "Mod3" "Mod5"]
  "Map of X11 modifier state flags to Emacs modifiers.

This should be a vector of 6 elements, with each element being a
string indicating the name of the X11 modifier state mask (sans
the -\"Mask\" suffix) corresponding to the Emacs modifiers Shift,
Control, Meta, Super, Hyper and Alt respectively.  nil indicates
that there is no mapping for this modifier."
  :type '(vector
	  (term-keys/x11-modifier :tag "Shift")
	  (term-keys/x11-modifier :tag "Control")
	  (term-keys/x11-modifier :tag "Meta")
	  (term-keys/x11-modifier :tag "Super")
	  (term-keys/x11-modifier :tag "Hyper")
	  (term-keys/x11-modifier :tag "Alt"))
  :group 'term-keys/x11)


(defun term-keys/x11-key-representable (keymap mods)
  "Return non-nil if the given KEYMAP + MODS vector is representable in X11."
  (and
   (elt keymap 1)                     ; Have X11 keysym?
   ;; Skip key combinations with unrepresentable modifiers
   (cl-reduce (lambda (x y) (and x y)) ; all
	      (mapcar (lambda (n)
			(or (not (elt mods n)) ; inactive modifier
			    (elt term-keys/x11-modifier-map n))) ; mapped
		      (number-sequence 0 (1- (length mods))))))) ; 0..5


(defun term-keys/x11-apply-mod-state (keymap shift lock control mod1 mod2 mod3 mod4 mod5)
  "Apply modifier state flags to an X11 KeySym.

Given a key (given in KEYMAP, a `term-keys/mapping' row) which
would be received by an application with no modifier flags,
return the KeySym that would be received by the application if
SHIFT, LOCK, CONTROL, MOD1, MOD2, MOD3, MOD4 and MOD5 modifier
flags are respectively active."
  (or
   (and shift (elt keymap 6))
   (elt keymap 1)))


(defun term-keys/x11-apply-mods (keymap mods)
  "Apply Emacs modifiers to KEYMAP.

Translate Emacs modifiers MODS to X11 modifiers (according to
`term-keys/x11-modifier-map') and invoke
`term-keys/x11-apply-mod-state').  KEYMAP is a
`term-keys/mapping' row."
  (let (shift lock control mod1 mod2 mod3 mod4 mod5)
    (mapc
     (lambda (n)
       (when (elt mods n)
	 (pcase (downcase (elt term-keys/x11-modifier-map n))
	   ("shift"
	    (setq shift t))
 	   ("lock"
	    (setq lock t))
 	   ("control"
	    (setq control t))
 	   ("mod1"
	    (setq mod1 t))
 	   ("mod2"
	    (setq mod2 t))
 	   ("mod3"
	    (setq mod3 t))
 	   ("mod4"
	    (setq mod4 t))
 	   ("mod5"
	    (setq mod5 t))
	   ('nil)
	   (_ (error "Unknown modifier: %s" (elt term-keys/x11-modifier-map n))))))
     (number-sequence 0 (1- (length mods))))
    (term-keys/x11-apply-mod-state keymap shift lock control mod1 mod2 mod3 mod4 mod5)))


(provide 'term-keys-x11)
;;; term-keys-x11.el ends here
