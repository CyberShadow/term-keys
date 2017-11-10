;;; term-keys-xterm.el --- term-keys support for xterm

;;; Commentary:

;; This file contains supplementary code for aiding in the
;; configuration of the xterm terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)


(defconst term-keys/xterm-modifier-names ["Shift" "Ctrl" "Meta" "Super" "Hyper" "Alt"]
  "Modifier key names in xterm key translation syntax.")


(defun term-keys/xterm-format-key (key mods)
  "Format key modifiers in xterm key translation syntax.

Returns the xterm translation string corresponding to the KEY and
modifier state MODS."
  (concat
   (cl-loop for modflag across mods
	    for index from 0
	    concat
	    (concat
	     (if modflag " " "~")
	     (elt term-keys/xterm-modifier-names index)
	     " "))
   "<Key> "
   key))


(defun term-keys/xterm-translations ()
  "Construct xterm configuration in the form of translation entries.

This function returns, as a list of strings (one string per
line), the xterm translation entries necessary to configure xterm
to encode term-keys key sequences, according to the term-keys
configuration."
  (term-keys/iterate-keys
   (lambda (index keymap mods)
     (format "%-55s: %s"
	     (term-keys/xterm-format-key (elt keymap 1) mods)
	     (mapconcat
	      (lambda (c) (format "string(0x%02x)" c))
	      (append
	       term-keys/prefix
	       (term-keys/encode-key index mods)
	       term-keys/suffix
	       nil)
	      " ")))))


(defun term-keys/xterm-xresources ()
  "Construct xterm configuration in the form of .Xresources entries.

This function returns, as a string, the .Xresources entries
necessary to configure xterm to encode term-keys key sequences,
according to the term-keys configuration.

The returned string is suitable to be added as-is to an
~/.Xresources file."
  (apply #'concat
	 "*VT100.Translations: #override \\\n"
	 (mapcar (lambda (s) (concat s " \\\n"))
		 (term-keys/xterm-translations))))


(defun term-keys/xterm-args ()
  "Construct xterm configuration in the form of command line arguments.

This function returns a list of xterm command line arguments
necessary to configure the terminal emulator to encode key
sequences, according to the term-keys configuration."
  (list
   "-xrm"
   (mapconcat #'identity
	      (cons
	       "XTerm.VT100.translations: #override"
	       (term-keys/xterm-translations))
	      "\\n")))


(defun term-keys/xterm-script ()
  "Construct xterm configuration in the form of a shell script.

This function returns, as a string, a shell script which launches
xterm configured to encode term-keys key sequences, according to
the term-keys configuration.

The returned string is suitable to be saved as-is in an
executable file and used for launching xterm."
  (concat
   "#!/bin/sh\n"
   "exec xterm \\\n\t"
   (mapconcat #'shell-quote-argument (term-keys/xterm-args) " \\\n\t")
   " \\\n\t\"$@\"\n"))


(defun term-keys/xterm-run-emacs ()
  "Launch Emacs via xterm enhanced with term-keys.

This function is used for testing and as an example."
  (apply #'call-process "xterm" nil nil nil
	 (append
	  (term-keys/xterm-args)
	  (list
	    "-e" (car command-line-args) "-nw"
	    "--load" (or load-file-name buffer-file-name)
	    "--funcall" "term-keys/init"
	    ))))


(provide 'term-keys-xterm)
;;; term-keys-xterm.el ends here
