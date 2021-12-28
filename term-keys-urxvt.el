;;; term-keys-urxvt.el --- term-keys support for urxvt

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
;; configuration of the urxvt terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)
(require 'term-keys-x11)


(defun term-keys/urxvt-format-mods (mods)
  "Format modifiers into an urxvt prefix,

Performs translation according to `term-keys/x11-modifier-map',
and rxvt's keysym_vocabulary.  To minimize the length of the
resulting command line, the shortest prefix is used."
  (mapconcat
     (lambda (n)
       (if (elt mods n)
	   (pcase (downcase (elt term-keys/x11-modifier-map n))
	     ("shift"     "S-")
	     ("lock"      "L-")
	     ("control"   "C-")
	     ("mod1"      "1-")
	     ("mod2"      "2-")
	     ("mod3"      "3-")
	     ("mod4"      "4-")
	     ("mod5"      "5-")
	     ;; These are private to urxvt:
	     ("meta"      "M-")
	     ("numlock"   "N-")
	     ("appkeypad" "K-")
	     ("level3"    "I-")
	     ;; Should be filtered ahead of time, using `term-keys/x11-key-representable'
	     ('nil (error "Unsupported modifier: #%d" n))
	     (_ (error "Unknown modifier: %s" (elt term-keys/x11-modifier-map n))))
	 ""))
     (number-sequence 0 (1- (length mods)))
     ""))


(defun term-keys/urxvt-format-key (keymap mods)
  "Format key modifiers in urxvt syntax.

Returns key (given in KEYMAP, a `term-keys/mapping' row)
prepended with appropriate modifiers depending on the elements of the bool vector
MODS, and performing translation as necessary.

If the KEY with MODS is unrepresentable, return nil."

  (when (term-keys/x11-key-representable keymap mods)
    (concat
     (term-keys/urxvt-format-mods mods)
     (term-keys/x11-apply-mods keymap mods))))


(defun term-keys/urxvt-args ()
  "Construct urxvt configuration in the form of command line arguments.

This function returns a list of urxvt (rxvt-unicode) command line
arguments necessary to configure the terminal emulator to encode
key sequences, according to the term-keys configuration."
  (apply #'nconc
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (list
	     (concat
	      "-keysym."
	      (term-keys/urxvt-format-key keymap mods))
	     (concat
	      "string:"
	      term-keys/prefix
	      (term-keys/encode-key index mods)
	      term-keys/suffix))))))


(defun term-keys/urxvt-script ()
  "Construct urxvt configuration in the form of a shell script.

This function returns, as a string, a shell script which launches
urxvt (rxvt-unicode) configured to encode term-keys key
sequences, according to the term-keys configuration.

The returned string is suitable to be saved as-is in an
executable file and used for launching urxvt."
  (concat
   "#!/bin/sh\n"
   "exec urxvt \\\n\t"
   (mapconcat #'shell-quote-argument (term-keys/urxvt-args) " \\\n\t")
   " \\\n\t\"$@\"\n"))


(defun term-keys/urxvt-xresources ()
  "Construct urxvt configuration in the form of .Xresources entries.

This function returns, as a string, the .Xresources entries
necessary to configure urxvt to encode term-keys key
sequences, according to the term-keys configuration.

The returned string is suitable to be added as-is to an
~/.Xresources file."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (format "URxvt.keysym.%s: string:%s%s%s\n"
		    (term-keys/urxvt-format-key keymap mods)
		    term-keys/prefix
		    (term-keys/encode-key index mods)
		    term-keys/suffix)))))


(defun term-keys/urxvt-run-emacs ()
  "Launch Emacs via urxvt enhanced with term-keys.

This function is used for testing and as an example."
  (apply #'start-process "urxvt" nil "urxvt"
	 (append
	  (term-keys/urxvt-args)
	  (list
	    "-e" (car command-line-args) "-nw"
	    "--load" term-keys/main-file-name
	    "--funcall" "term-keys/init"
	    ))))


(provide 'term-keys-urxvt)
;;; term-keys-urxvt.el ends here
