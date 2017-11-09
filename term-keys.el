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
  '(["<escape>"		"Escape"	1	"Esc"		]
    ["<f1>"		"F1"		59	"F1"		]
    ["<f2>"		"F2"		60	"F2"		]
    ["<f3>"		"F3"		61	"F3"		]
    ["<f4>"		"F4"		62	"F4"		]
    ["<f5>"		"F5"		63	"F5"		]
    ["<f6>"		"F6"		64	"F6"		]
    ["<f7>"		"F7"		65	"F7"		]
    ["<f8>"		"F8"		66	"F8"		]
    ["<f9>"		"F9"		67	"F9"		]
    ["<f10>"		"F10"		68	"F10"		]
    ["<f11>"		"F11"		87	"F11"		]
    ["<f12>"		"F12"		88	"F12"		]
    ["<print>"		"Print"		99	"Print"		]
    ["<Scroll_Lock>"	"Scroll_Lock"	70	"ScrollLock"	]
    ["<pause>"		"Pause"		119	"Pause"		]

    ["`"		"grave"		43	"`"		]
    ["1"		"1"		2	"1"		]
    ["2"		"2"		3	"2"		]
    ["3"		"3"		4	"3"		]
    ["4"		"4"		5	"4"		]
    ["5"		"5"		6	"5"		]
    ["6"		"6"		7	"6"		]
    ["7"		"7"		8	"7"		]
    ["8"		"8"		9	"8"		]
    ["9"		"9"		10	"9"		]
    ["0"		"0"		11	"0"		]
    ["-"		"minus"		12	"-"		]
    ["="		"equal"		13	"="		]
    ["<backspace>"	"BackSpace"	14	"Backspace"	]
    ["<tab>"		"Tab"		15	"Tab"		]
    ["q"		"q"		16	"Q"		]
    ["w"		"w"		17	"W"		]
    ["e"		"e"		18	"E"		]
    ["r"		"r"		19	"R"		]
    ["t"		"t"		20	"T"		]
    ["y"		"y"		21	"Y"		]
    ["u"		"u"		22	"U"		]
    ["i"		"i"		23	"I"		]
    ["o"		"o"		24	"O"		]
    ["p"		"p"		25	"P"		]
    ["["		"bracketleft"	26	"["		]
    ["]"		"bracketright"	27	"]"		]
    ["<return>"		"Return"	28	"Return"	]
    ["<Caps_Lock>"	"Caps_Lock"	58	"CapsLock"	]
    ["a"		"a"		30	"A"		]
    ["s"		"s"		31	"S"		]
    ["d"		"d"		32	"D"		]
    ["f"		"f"		33	"F"		]
    ["g"		"g"		34	"G"		]
    ["h"		"h"		35	"H"		]
    ["j"		"j"		36	"J"		]
    ["k"		"k"		37	"K"		]
    ["l"		"l"		38	"L"		]
    [";"		"semicolon"	39	";"		]
    ["'"		"apostrophe"	40	"'"		]
    [nil		"Shift_L"	42	"Shift"		]
    ["\\"		"backslash"	43	"\\"		]
    ["z"		"z"		44	"Z"		]
    ["x"		"x"		45	"X"		]
    ["c"		"c"		46	"C"		]
    ["v"		"v"		47	"V"		]
    ["b"		"b"		48	"B"		]
    ["n"		"n"		49	"N"		]
    ["m"		"m"		50	"M"		]
    [","		"comma"		51	","		]
    ["."		"period"	52	"."		]
    ["/"		"slash"		53	"/"		]
    [nil		"Shift_R"	54	"Shift"		]
    [nil		"Ctrl_L"	29	"Ctrl"		]
    [nil		"Super_L"	125	"Meta"		]
    [nil		"Alt_L"		56	"Alt"		]
    ["SPC"		"space"		57	"Space"		]
    [nil		"Alt_R"		100	"Alt"		]
    [nil		"Super_R"	126	"Meta"		]
    ["<menu>"		"Menu"		127	"Menu"		]
    [nil		"Ctrl_R"	97	"Ctrl"		]

    ["<up>"		"Up"		103	"Up"		]
    ["<down>"		"Down"		108	"Down"		]
    ["<left>"		"Left"		105	"Left"		]
    ["<right>"		"Right"		106	"Right"		]

    ["<insert>"		"Insert"	110	"Ins"		]
    ["<delete>"		"Delete"	111	"Del"		]
    ["<home>"		"Home"		102	"Home"		]
    ["<end>"		"End"		107	"End"		]
    ["<prior>"		"Prior"		104	"PgUp"		]
    ["<next>"		"Next"		109	"PgDown"	]

    ;; Add new entries at the end of the list, to avoid disrupting
    ;; existing configurations.

    ;; TODO: numpad
    )
  "List of keys supported by the `term-keys' package.

Each item in the list is a 4-element vector:

The first element is the Emacs key name, as it occurs in
`describe-key' or `kbd'.  nil can be used to indicate keys which
Emacs currently does not recognize (but are still known by other
input systems), such as modifier keys (which Emacs can't process
on its own, only in combination with a non-modifier key).

The second element is the X11 KeySym name, as returned by
XKeysymToString.  Used for urxvt/xterm configuration.

The third element is the keynumber (keycode) from the Linux TTY.
You can obtain a key's keynumber by running the 'showkey' program
in a TTY.

The fourth element is the Qt key name, as returned by
QKeySequence::toString and accepted by QKeySequence::fromString.
An easy way to obtain their name is using any KDE application's
\"Configure Shortcuts\" dialog."
  :type '(repeat
	  (vector
	   :tag "Key mapping"
	   (choice
	    :tag "Emacs key"
	    (const
	     :tag "No corresponding Emacs key"
	     nil)
	    (string
	     :tag "Emacs key name"))
	   (string
	    :tag "X11 KeySym")
	   (integer
	    :tag "Linux TTY keynumber")
	   (string
	    :tag "Qt key name")))
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


(defun term-keys/want-key-p-def (key mods)
  "Default implementation for `term-keys/want-key-p-func'.

This function controls which key combinations are to be encoded
and decoded by default using the term-keys protocol extension.
KEY is the KeySym name as listed in `term-keys/mapping'; MODS is
a 6-element bool vector representing the modifiers Shift /
Control / Meta / Super / Hyper / Alt respectively, with t or nil
representing whether they are depressed or not.  Returns non-nil
if the specified key combination should be encoded.

Note that the ALT modifier rarely actually corresponds to the Alt
key on PC keyboards; the META modifier will usually be used
instead."
  (let ((shift   (elt mods 0))
	(control (elt mods 1))
	(meta    (elt mods 2))
	(super   (elt mods 3))
	(hyper   (elt mods 4))
	(alt     (elt mods 5)))
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
      ))))

(defcustom term-keys/want-key-p-func 'term-keys/want-key-p-def
  "Function for deciding whether to encode a key combination.

This should be set to a function with the same signature and
semantics as `term-keys/want-key-p-def'.  Look at that function's
documentation for more details.

Customize this variable to a function or lambda defined by you to
change which key combinations to encode."
  :type 'function
  :group 'term-keys)


(defconst term-keys/modifier-chars "SCMsHA"
  "The characters for the Emacs modifiers supported by term-keys.")


(defun term-keys/format-key (key mods)
  "Format key modifiers in Emacs/urxvt syntax.

Returns KEY prepended with S-, C-, M-, s-, H-, or A- depending on
the elements of the bool vector MODS are correspondingly
non-nil."
  (concat
   (cl-loop for modflag across mods
	    for index from 0
	    if modflag
	    concat (concat (string (elt term-keys/modifier-chars index))
			   "-"))
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


(defun term-keys/encode-key (key mods)
  "Encode a key combination to term-keys' protocol.

Returns a string ready to be sent by a terminal emulator (or
received by Emacs running in a terminal) which encodes the
combination of KEY (the key's index in the `term-keys/mapping'
table) and the modifiers MODS (a 6-element bool vector indicating
whether the respective modifier is pressed or not)."
  (term-keys/encode-number
   (cl-loop for index from 0
	    for factor = 1 then (* factor 2)
	    for modflag across mods
	    if modflag
	    sum factor into modflags
	    finally return (+ modflags (* factor key)))))


(defun term-keys/iterate-keys (fun)
  "Call FUN over every enabled key combination.

Iterate over all elements of `term-keys/mapping' and modifier key
combinations, filter the enabled ones using
`term-keys/want-key-p-func', and call (FUN INDEX KEYMAP MODS),
where INDEX is the key index in `term-keys/mapping', KEYMAP is
the `term-keys/mapping' element vector at that index, and MODS is
a bool vector for the active modifier keys.

Collect FUN's return values in a list and return it."
  (cl-loop
   for keymap in term-keys/mapping
   for index from 0
   append
   (cl-loop
    ;; Iterate from 0 to 2^64-1 for a bitmask of all modifier combinations
    for modnum from 0 to (1- (lsh 1 (length term-keys/modifier-chars)))
    ;; Convert the integer bitmask to a bool-vector
    for mods = (apply #'bool-vector (mapcar (lambda (n) (not (zerop (logand modnum (lsh 1 n)))))
					    (number-sequence 0 (1- (length term-keys/modifier-chars)))))
    if (and
	(elt keymap 0)                  ; Representable in Emacs?
	(funcall term-keys/want-key-p-func (elt keymap 1) mods)) ; Want this key combination?
    collect (funcall fun index keymap mods))))


;;;###autoload
(defun term-keys/init ()
  "Set up configured key sequences for the current terminal."
  (interactive)
  (term-keys/iterate-keys
   (lambda (index keymap mods)
     (define-key
       input-decode-map
       (concat
	term-keys/prefix
	(term-keys/encode-key index mods)
	term-keys/suffix)
       (kbd (term-keys/format-key
	     (elt keymap 0) mods))))))


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


(defun term-keys/urxvt-format-key (key mods)
  "Format key modifiers in urxvt syntax.

Returns KEY prepended with S-, C-, M-, s-, H-, or A- depending on
the elements of the bool vector MODS are correspondingly non-nil,
additionally upcasing letter keys."
  (if (and (elt mods 0)                 ; Shift
	   (string-match-p "^[a-z]$" key))
      ;; Upcase letter keys
      (term-keys/format-key (upcase key) (bool-vector nil
						      (elt mods 1)
						      (elt mods 2)
						      (elt mods 3)
						      (elt mods 4)
						      (elt mods 5)))
    (term-keys/format-key key mods)))


(defun term-keys/urxvt-args ()
  "Construct urxvt configuration in the form of command line arguments.

This function returns a list of urxvt (rxvt-unicode) command line
arguments necessary to configure the terminal emulator to encode
key sequences (as configured by `term-keys/want-key-p-func')."
  (apply #'nconc
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)
	    (list
	     (concat
	      "-keysym."
	      (term-keys/urxvt-format-key (elt keymap 1) mods))
	     (concat
	      "string:"
	      term-keys/prefix
	      (term-keys/encode-key index mods)
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
	  (lambda (index keymap mods)
	    (format "URxvt.keysym.%s: string:%s%s%s\n"
		    (term-keys/urxvt-format-key (elt keymap 1) mods)
		    term-keys/prefix
		    (term-keys/encode-key index mods)
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
to encode term-keys key sequences (as configured by
`term-keys/want-key-p-func')."
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


;; Linux TTY


(define-widget 'term-keys/linux-modifier 'lazy
  "Choice for Linux modifiers for keymap files."
  :type '(choice (const "Shift")
		 (const "AltGr")
		 (const "Control")
		 (const "Alt")
		 (const "ShiftL")
		 (const "ShiftR")
		 (const "CtrlL")
		 (const "CtrlR")
		 (const "CapsShift")
		 (const :tag "(none)" nil)))


(defcustom term-keys/linux-modifier-map ["Shift" "Control" "Alt" nil nil "AltGr"]
  "Modifier keys for Linux TTY keymaps.

This should be a vector of 6 elements, with each element being a
string indicating the name of the modifier key corresponding to
the Emacs modifiers Shift, Control, Meta, Super, Hyper and Alt
respectively, as they should appear in generated .keymap files.
nil indicates that there is no mapping for this modifier."
  :type '(vector
	  (term-keys/linux-modifier :tag "Shift")
	  (term-keys/linux-modifier :tag "Control")
	  (term-keys/linux-modifier :tag "Meta")
	  (term-keys/linux-modifier :tag "Super")
	  (term-keys/linux-modifier :tag "Hyper")
	  (term-keys/linux-modifier :tag "Alt"))
  :group 'term-keys)


(defcustom term-keys/linux-first-function-key 13
  "First Linux TTY keymap function key to use for term-keys bindings.

The Linux TTY allows binding custom character sequences to keys
only by assigning them to a \"function key\" (named thus after
the usual F1, F2 etc. function keys found on a computer
keyboard).  Although most PC keyboards today only have 12
function keys, some keyboards/computers had more, and (also for
the purposes of key binding) the Linux kernel allows registering
character sequences for as many as 246 function keys (see output
for 'loadkeys -l' for the exact figure on your machine).
Crucially, these virtual keys provide the means to configure
arbitrary character sequences for any key combination, which is
necessary for term-keys to work.

This variable specifies the first function key entry that's used
by term-keys in the .keymap files it generates (see
`term-keys/linux-keymap').  The default value (13) will make
`term-keys' use F13 and onwards, and is fine for most uses,
unless you happen to use a keyboard with more than 12 F-keys, or
wish to use the F13-F20 keys through the shifted state of the
F1-F8 keys."
  :type 'integer
  :group 'term-keys)


(defun term-keys/linux-keymap ()
  "Construct Linux TTY configuration in the form of a keymap.

This function returns, as a string, a keymap which can be used to
configure the Linux kernel's TTY emulator to encode term-keys key
sequences (as configured by `term-keys/want-key-p-func').

The returned string is suitable to be saved as-is in a .keymap
file and loaded by the loadkeys program."
  (apply #'concat
	 (let ((fkey term-keys/linux-first-function-key))
	   (term-keys/iterate-keys
	    (lambda (index keymap mods)

	      ;; Skip key combinations with unrepresentable modifiers
	      (unless (cl-reduce (lambda (x y) (or x y)) ; any
				 (mapcar (lambda (n) ; active modifier mapped to nil
					   (and (elt mods n)
						(not (elt term-keys/linux-modifier-map n))))
					 (number-sequence 0 (1- (length mods))))) ; 0..5
		(prog1
		    (format "# %s\n%s\tkeycode %3d = F%d\nstring F%d = \"%s\"\n\n"
			    ;; Emacs key name for comment
			    (term-keys/format-key (elt keymap 0) mods)

			    (if (cl-reduce (lambda (x y) (or x y)) mods)
				;; tab-separated mod list
				(mapconcat
				 (lambda (n)
				   (if (elt mods n) (elt term-keys/linux-modifier-map n) ""))
				 (number-sequence 0 (1- (length mods)))
				 "\t")
			      ;; "plain" if no mods
			      (concat "plain" (make-string (1- (length mods)) ?\t)))
			    (elt keymap 2) ; keynumber
			    fkey        ; F-key number (use)
			    fkey        ; F-key number (declaration)
			    (mapconcat  ; octal-escaped sequence
			     (lambda (x) (format "\\%03o" x))
			     (append
			      term-keys/prefix
			      (term-keys/encode-key index mods)
			      term-keys/suffix
			      nil)
			     ""))
		  (setq fkey (1+ fkey)))))))))


;; Konsole


(define-widget 'term-keys/konsole-modifier 'lazy
  "Choice for Konsole key binding modifiers and state flags."
  :type '(choice (const "Shift")
		 (const "Ctrl")
		 (const "Alt")
		 (const "Meta")
		 (const "KeyPad")
		 (const "AppScreen")
		 (const "AppCursorKeys")
		 (const "NewLine")
		 (const "Ansi")
		 (const "AnyModifier")
		 (const "AppKeypad")
		 (const :tag "(none)" nil)))


(defcustom term-keys/konsole-modifier-map ["Shift" "Ctrl" "Alt" "Meta" nil nil]
  "Modifier keys for Konsole key bindings.

This should be a vector of 6 elements, with each element being a
string indicating the name of the Konsole modifier or state flag
corresponding to the Emacs modifiers Shift, Control, Meta, Super,
Hyper and Alt respectively, as they should appear in generated
Konsole .keytab files.  nil indicates that there is no mapping
for this modifier."
  :type '(vector
	  (term-keys/konsole-modifier :tag "Shift")
	  (term-keys/konsole-modifier :tag "Control")
	  (term-keys/konsole-modifier :tag "Meta")
	  (term-keys/konsole-modifier :tag "Super")
	  (term-keys/konsole-modifier :tag "Hyper")
	  (term-keys/konsole-modifier :tag "Alt"))
  :group 'term-keys)


(defun term-keys/konsole-keytab ()
  "Construct Konsole key binding configuration as .keytab file syntax.

This function returns, as a string, a Konsole keytab which can be
used to configure Konsole to encode term-keys key sequences (as
configured by `term-keys/want-key-p-func').

The returned string is suitable to be pasted as-is to the end of
an existing Konsole .keytab file."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)

	    ;; Skip key combinations with unrepresentable modifiers
	    (unless (cl-reduce (lambda (x y) (or x y)) ; any
			       (mapcar (lambda (n) ; active modifier mapped to nil
					 (and (elt mods n)
					      (not (elt term-keys/konsole-modifier-map n))))
				       (number-sequence 0 (1- (length mods))))) ; 0..5
	      (format "key %s%s : \"%s\"\n"
		      (elt keymap 3) ; key name
		      (mapconcat
		       (lambda (n)
			 (if (elt term-keys/konsole-modifier-map n)
			     (concat
			      (if (elt mods n) "+" "-")
			      (elt term-keys/konsole-modifier-map n))
			   ""))
		       (number-sequence 0 (1- (length mods)))
		       "")
		      (mapconcat  ; hex-escaped sequence
		       (lambda (x) (format "\\x%02X" x))
		       (append
			term-keys/prefix
			(term-keys/encode-key index mods)
			term-keys/suffix
			nil)
		       "")))))))


;; St


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
Control, Meta, Super, Hyper and Alt respectively, as they should
appear in the generated st configuration.  nil indicates that
there is no mapping for this modifier."
  :type '(vector
	  (term-keys/x11-modifier :tag "Shift")
	  (term-keys/x11-modifier :tag "Control")
	  (term-keys/x11-modifier :tag "Meta")
	  (term-keys/x11-modifier :tag "Super")
	  (term-keys/x11-modifier :tag "Hyper")
	  (term-keys/x11-modifier :tag "Alt"))
  :group 'term-keys)


(defun term-keys/x11-apply-mod-state (key shift lock control mod1 mod2 mod3 mod4 mod5)
  "Apply modifier state flags to an X11 KeySym.

Given a KeySym KEY which would be received by an application with
no modifier flags, return the KeySym that would be received by
the application if SHIFT, LOCK, CONTROL, MOD1, MOD2, MOD3, MOD4
and MOD5 modifier flags are respectively active."
  (cond
   ((and (string-match-p "^[a-z]$" key) shift)
    (upcase key))
   ((string-equal key "Tab")
    "ISO_Left_Tab")
   (t
    key)))


(defun term-keys/x11-apply-mods (key mods)
  "Apply Emacs modifiers to X11 KeySym KEY.

Translate Emacs modifiers MODS to X11 modifiers (according to
`term-keys/x11-modifier-map') and invoke
`term-keys/x11-apply-mod-state')."
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
    (term-keys/x11-apply-mod-state key shift lock control mod1 mod2 mod3 mod4 mod5)))


(defun term-keys/st-config-key ()
  "Construct st key binding configuration (key array entries).

This function returns, as a string, C code of Key entries of the
config.h 'key' array, which can be used to configure st to encode
term-keys key sequences (as configured by
`term-keys/want-key-p-func').

The returned string is suitable to be pasted as-is into the 'key'
array in the st config.h configuration file; however, this is
just one half of the necessary configuration (see
`term-keys/st-config-mappedkeys' for the other half)."
  (apply #'concat
	 (term-keys/iterate-keys
	  (lambda (index keymap mods)

	    ;; Skip key combinations with unrepresentable modifiers
	    (unless (cl-reduce (lambda (x y) (or x y)) ; any
			       (mapcar (lambda (n) ; active modifier mapped to nil
					 (and (elt mods n)
					      (not (elt term-keys/x11-modifier-map n))))
				       (number-sequence 0 (1- (length mods))))) ; 0..5
	      (format "{ XK_%-16s, %-40s, \"%s\", 0, 0, 0},\n"
		      (term-keys/x11-apply-mods (elt keymap 1) mods) ; X11 key name
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
st to encode term-keys key sequences (as configured by
`term-keys/want-key-p-func').

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
		     (term-keys/x11-apply-mods (elt keymap 1) mods)))))))


(provide 'term-keys)
;;; term-keys.el ends here
