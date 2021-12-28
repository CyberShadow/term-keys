;;; term-keys-st.el --- term-keys support for the st terminal emulator

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
;; configuration of the st terminal emulator to interoperate with the
;; term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)
(require 'term-keys-x11)


(defun term-keys/st-config-key ()
  "Construct st key binding configuration (key array entries).

This function returns, as a string, C code of Key entries of the
config.h 'key' array, which can be used to configure st to encode
term-keys key sequences, according to the term-keys
configuration.

The returned string is suitable to be pasted as-is into the 'key'
array in the st config.h configuration file; however, this is
just one half of the necessary configuration (see
`term-keys/st-config-mappedkeys' for the other half)."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)

	    (when (term-keys/x11-key-representable keymap mods)
	      (format "{ XK_%-16s, %-40s, \"%s\", 0, 0},\n"
		      (term-keys/x11-apply-mods keymap mods) ; X11 key name
		      (if (cl-reduce (lambda (x y) (or x y)) mods)
			  (mapconcat
			   (lambda (n)
			     (concat
			      (elt term-keys/x11-modifier-map n)
			      "Mask"))
			   (cl-remove-if-not (lambda (n) (elt mods n))
					     (number-sequence 0 (1- (length mods))))
			   "|")
			"XK_NO_MOD")
		      (mapconcat  ; hex-escaped sequence
		       (lambda (x) (format "\\x%02X" x))
		       (append
			term-keys/prefix
			(term-keys/encode-key index mods)
			term-keys/suffix
			nil)
		       "")))))))


(defun term-keys/st-config-mappedkeys ()
  "Construct st key binding configuration (mappedkeys array entries).

This function returns, as a string, C code of KeySym entries of
the config.h 'mappedkeys' array, which can be used to configure
st to encode term-keys key sequences, according to the term-keys
configuration.

The returned string is suitable to be pasted as-is into the
'mappedkeys' array in the st config.h configuration file;
however, this is just one half of the necessary
configuration (see `term-keys/st-config-key' for the other
half)."
  (apply #'concat
	 (delete-dups
	  (term-keys/iterate-keys
	   (lambda (index keymap mods)
	     (format "XK_%s,\n"
		     (term-keys/x11-apply-mods keymap mods)))))))


(provide 'term-keys-st)
;;; term-keys-st.el ends here
