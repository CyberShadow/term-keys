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

(require 'cl-lib)

(defgroup term-keys nil
  "The `term-keys' customization group.

This package allows lossless keyboard input when using Emacs from
a terminal emulator.

For more information, please see the accompanying README.md
file."
  :group 'convenience)

(defcustom term-keys/mapping
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
    )
  "List of keys supported by the `term-keys' package.

TODO: Finalize and document structure"
  :type '(repeatn
	  (cons
	   :tag "Key mapping"
	   (string
	    :tag "X11 KeySym")
	   (choice
	    :tag "Emacs key"
	    (const
	     :tag "No corresponding Emacs key"
	     nil)
	    (string
	     :tag "Emacs key name"))))
  :group 'term-keys)



(defcustom term-keys/prefix "\033\037"
  "Key sequence prefix.

Indicates the byte string to be sent before a term-keys key code.

The default value is \\033\\037 (0x1B 0x1F, or ^[^_).

The prefix, or any starting substring of it, or any sequence
beginning with it, should not be already bound to an action in
Emacs.  E.g. with the default, neither ^[, ^[^_, or ^[^_abc
should by themselves be bound to an Emacs action."
  :group 'term-keys)


(defcustom term-keys/suffix "\037"
  "Key sequence suffix.

Indicates the end of the data encoding the pressed key
combination.  Can be any character which isn't used in the
`term-keys/encode-number' encoding scheme."
  :group 'term-keys)


(defun term-keys/want-key-p-def (key shift control meta super hyper alt)
  "Default implementation for `term-keys/want-key-p-func'.

This function controls which key combinations are to be encoded
and decoded by default using the term-keys protocol extension.
KEY is the KeySym name as listed in `term-keys/mapping'; the
modifiers SHIFT / CONTROL / META / SUPER / HYPER / ALT are t or
nil depending on whether they are depressed or not.  Returns
non-nil if the specified key combination should be encoded.

Note that the ALT modifier rarely actually corresponds to the Alt
key on PC keyboards; the META modifier will usually be used
instead."
  (and

   ;; We don't care about Super/Hyper/Alt modifiers
   (not super)
   (not hyper)
   (not alt)

   (or
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2004-03/msg00306.html
    (and (string-equal key "g") control meta)

    ;; Navigation keys and Control
    (and (member key '("Up" "Down" "Left" "Right" "Home" "End" "Prior" "Next")) control (or shift meta))

    ;; S-PgUp/PgDn - usually used for scrolling the terminal, but not useful in Emacs
    (and (member key '("Prior" "Next")) shift)

    ;; Ctrl+Tab
    (and (string-equal key "Tab") control)

    ;; Ctrl+BackSpace
    (and (string-equal key "BackSpace") control)

    ;; C-S-x is unrepresentable for letters
    (and (string-match-p "^[a-z]$" key) control shift)

    ;; C-x is unrepresentable for digits
    (and (string-match-p "^[0-9]$" key) control)

    ;; Menu (Apps) key
    (string-equal key "Menu")
    )))

(defcustom term-keys/want-key-p-func 'term-keys/want-key-p-def
  "Function for deciding whether to encode a key combination.

This should be set to a function with the same signature and
semantics as `term-keys/want-key-p-def'.  Look at that function's
documentation for more details.

Customize this variable to a function or lambda defined by you to
change which key combinations to encode."
  :type 'function
  :group 'term-keys)


(defun term-keys/format-key (key shift control meta super hyper alt)
  "Format key modifiers in Emacs/urxvt syntax.

Returns KEY prepended with S-, C-, M-, s-, H-, or A- depending on
whether SHIFT, CONTROL, META, SUPER, HYPER, or ALT are
correspondingly non-nil."
  (concat
   (if shift   "S-" "")
   (if control "C-" "")
   (if meta    "M-" "")
   (if super   "s-" "")
   (if hyper   "H-" "")
   (if alt     "A-" "")
   key))


(defun term-keys/encode-number (num)
  "Efficiently encode non-negative integer NUM into a string.

Use only characters that can safely occur on a command line or
configuration file.  Current implementation uses base-96 (ASCII
\x20 .. \x7F)."
  (apply #'string
	 (nreverse (cl-loop while (not (zerop num))
			       collect (+ 32 (% num 96))
			       do (setq num (/ num 96))))))


(defun term-keys/decode-number (str)
  "Decode a string STR encoded by `term-keys/encode-number'."
  (cl-do ((bytes (append str nil)
		 (cdr bytes))
	  (num 0 (+ (* num 96) (- (car bytes) 32))))
      ((not bytes) num)))

(cl-loop for n in '(0 1 95 96 97 12345 123456789)
	 do (cl-assert (eq (term-keys/decode-number
			    (term-keys/encode-number n)) n)))


(defun term-keys/encode-key (key shift control meta super hyper alt)
  "Encode a key combination to term-keys' protocol.

Returns a string ready to be sent by a terminal emulator (or
received by Emacs running in a terminal) which encodes the
combination of KEY (the key's index in the `term-keys/mapping'
table) and SHIFT, CONTROL, META, SUPER, HYPER, or ALT (indicating
whether they're pressed or not)."
  (term-keys/encode-number
   (+
    (if shift 1 0)
    (if control 2 0)
    (if meta 4 0)
    (if super 8 0)
    (if hyper 16 0)
    (if alt 32 0)
    (* 64 key))))


(defun term-keys/iterate-keys (fun)
  "Call FUN over every enabled key combination.

Iterate over all elements of `term-keys/mapping' and
modifier key combinations, filter the enabled ones using
`term-keys/want-key-p-func', and call (FUN INDEX PAIR SHIFT
CONTROL META SUPER HYPER ALT).

Collect FUN's return values in a list and return it."
  (cl-loop
   for pair in term-keys/mapping
   for index from 0
   append
   (cl-loop
    for mods from 0 to 63
    for shift   = (not (zerop (logand mods  1)))
    for control = (not (zerop (logand mods  2)))
    for meta    = (not (zerop (logand mods  4)))
    for super   = (not (zerop (logand mods  8)))
    for hyper   = (not (zerop (logand mods 16)))
    for alt     = (not (zerop (logand mods 32)))
    if (and
	(cdr pair)
	(funcall term-keys/want-key-p-func (car pair)
		 shift control meta super hyper alt))
    collect (funcall fun index pair
		     shift control meta super hyper alt))))


;;;###autoload
(defun term-keys/init ()
  "Set up configured key sequences for the current terminal."
  (term-keys/iterate-keys
   (lambda (index pair shift control meta super hyper alt)
     (define-key
       input-decode-map
       (concat
	term-keys/prefix
	(term-keys/encode-key index shift control meta super hyper alt)
	term-keys/suffix)
       (kbd (term-keys/format-key
	     (cdr pair) shift control meta super hyper alt))))))


;;;###autoload
(define-minor-mode term-keys-mode
  "`term-keys' global minor mode.

When enabled, automatically set up configured keys for new frames
on TTY terminals.  If the current frame is on a TTY, set it up as
well."
  :global t
  (if term-keys-mode
      (progn
	(add-hook 'tty-setup-hook 'term-keys/init)
	(if (eq (framep-on-display) t)
	    (term-keys/init)))
    (remove-hook 'tty-setup-hook 'term-keys/init)))


;; urxvt


(defun term-keys/urxvt-format-key (key shift control meta super hyper alt)
  "Format key modifiers in urxvt syntax.

Returns KEY prepended with S-, C-, M-, s-, H-, or A- depending on
whether SHIFT, CONTROL, META, SUPER, HYPER, or ALT are
correspondingly non-nil, additionally upcasing letter keys."
  (if (and shift (string-match-p "^[a-z]$" key))
      ;; Upcase letter keys
      (term-keys/format-key (upcase key) nil control meta super hyper alt)
    (term-keys/format-key key shift control meta super hyper alt)))


(defun term-keys/urxvt-args ()
  "Construct urxvt configuration in the form of command line arguments.

This function returns a list of urxvt (rxvt-unicode) command line
arguments necessary to configure the terminal emulator to encode
key sequences (as configured by `term-keys/want-key-p-func')."
  (apply #'nconc
	 (term-keys/iterate-keys
	  (lambda (index pair shift control meta super hyper alt)
	    (list
	     (concat
	      "-keysym."
	      (term-keys/urxvt-format-key (car pair) shift control meta super hyper alt))
	     (concat
	      "string:"
	      term-keys/prefix
	      (term-keys/encode-key index shift control meta super hyper alt)
	      term-keys/suffix))))))


(defun term-keys/urxvt-script ()
  "Construct urxvt configuration in the form of a shell script.

This function returns, as a string, a shell script which launches
urxvt (rxvt-unicode) configured to encode term-keys key
sequences (as configured by `term-keys/want-key-p-func').

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
sequences (as configured by `term-keys/want-key-p-func').

The returned string is suitable to be added as-is to an
~/.Xresources file."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index pair shift control meta super hyper alt)
	    (format "URxvt.keysym.%s: string:%s%s%s\n"
		    (term-keys/urxvt-format-key (car pair) shift control meta super hyper alt)
		    term-keys/prefix
		    (term-keys/encode-key index shift control meta super hyper alt)
		    term-keys/suffix)))))


(defun term-keys/urxvt-run-emacs ()
  "Launch Emacs via urxvt enhanced with term-keys.

This function is used for testing and as an example."
  (apply #'call-process "urxvt" nil nil nil
	 (append
	  (term-keys/urxvt-args)
	  (list
	    "-e" (car command-line-args) "-nw"
	    "--load" (or load-file-name buffer-file-name)
	    "--funcall" "term-keys/init"
	    ))))


;; xterm


(defun term-keys/xterm-format-key (key shift control meta super hyper alt)
  "Format key modifiers in xterm key translation syntax.

Returns the xterm translation string corresponding to the KEY and
modifier state SHIFT, CONTROL, META, SUPER, HYPER, and ALT."
  (concat
   (if shift   " " "~") "Shift "
   (if control " " "~") "Ctrl "
   (if meta    " " "~") "Meta "
   (if super   " " "~") "Super "
   (if hyper   " " "~") "Hyper "
   (if alt     " " "~") "Alt "
   "<Key> "
   ;; (if (and shift (string-match-p "^[a-z]$" key))
   ;;    ;; Upcase letter keys
   ;;     (upcase key)
   ;;   key)))
   key))


(defun term-keys/xterm-translations ()
  "Construct xterm configuration in the form of translation entries.

This function returns, as a list of strings (one string per
line), the xterm translation entries necessary to configure xterm
to encode term-keys key sequences (as configured by
`term-keys/want-key-p-func')."
  (term-keys/iterate-keys
   (lambda (index pair shift control meta super hyper alt)
     (format "%-55s: %s"
	     (term-keys/xterm-format-key (car pair) shift control meta super hyper alt)
	     (mapconcat
	      (lambda (c) (format "string(0x%02x)" c))
	      (append
	       term-keys/prefix
	       (term-keys/encode-key index shift control meta super hyper alt)
	       term-keys/suffix
	       nil)
	      " ")))))


(defun term-keys/xterm-xresources ()
  "Construct xterm configuration in the form of .Xresources entries.

This function returns, as a string, the .Xresources entries
necessary to configure xterm to encode term-keys key
sequences (as configured by `term-keys/want-key-p-func').

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
sequences (as configured by `term-keys/want-key-p-func')."
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
xterm configured to encode term-keys key sequences (as configured
by `term-keys/want-key-p-func').

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


(provide 'term-keys)
;;; term-keys.el ends here
