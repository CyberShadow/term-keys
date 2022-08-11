;;; term-keys.el --- Lossless keyboard input in a terminal emulator

;; Version: 0.1.0
;; Author: Vladimir Panteleev
;; Url: https://github.com/CyberShadow/term-keys
;; Keywords: terminals
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
  "Lossless keyboard input in a terminal emulator.

term-keys allows lossless keyboard input when using Emacs from a
terminal emulator.

For more information, please see the accompanying README.md
file."
  :group 'convenience)


(defcustom term-keys/mapping
  ;; Emacs		X11		TTY	Qt		macOS	Emacs shifted	X11 shifted	GLFW		winit		Qt sh.	macOS-S	macOS-C Wezterm

  '(["<escape>"		"Escape"	1	"Esc"		#x001B	nil		nil		"ESCAPE"	"Escape"	nil	nil	#x001b	"Escape"	]
    ["<f1>"		"F1"		59	"F1"		#xF704	nil		nil		"F1"		"F1"		nil	nil	nil	"F1"		]
    ["<f2>"		"F2"		60	"F2"		#xF705	nil		nil		"F2"		"F2"		nil	nil	nil	"F2"		]
    ["<f3>"		"F3"		61	"F3"		#xF706	nil		nil		"F3"		"F3"		nil	nil	nil	"F3"		]
    ["<f4>"		"F4"		62	"F4"		#xF707	nil		nil		"F4"		"F4"		nil	nil	nil	"F4"		]
    ["<f5>"		"F5"		63	"F5"		#xF708	nil		nil		"F5"		"F5"		nil	nil	nil	"F5"		]
    ["<f6>"		"F6"		64	"F6"		#xF709	nil		nil		"F6"		"F6"		nil	nil	nil	"F6"		]
    ["<f7>"		"F7"		65	"F7"		#xF70A	nil		nil		"F7"		"F7"		nil	nil	nil	"F7"		]
    ["<f8>"		"F8"		66	"F8"		#xF70B	nil		nil		"F8"		"F8"		nil	nil	nil	"F8"		]
    ["<f9>"		"F9"		67	"F9"		#xF70C	nil		nil		"F9"		"F9"		nil	nil	nil	"F9"		]
    ["<f10>"		"F10"		68	"F10"		#xF70D	nil		nil		"F10"		"F10"		nil	nil	nil	"F10"		]
    ["<f11>"		"F11"		87	"F11"		#xF70E	nil		nil		"F11"		"F11"		nil	nil	nil	"F11"		]
    ["<f12>"		"F12"		88	"F12"		#xF70F	nil		nil		"F12"		"F12"		nil	nil	nil	"F12"		]
    ["<print>"		"Print"		99	"Print"		#xF710	nil		nil		"PRINT_SCREEN"	"Snapshot"	nil	nil	nil	"Print"		]
    ["<Scroll_Lock>"	"Scroll_Lock"	70	"ScrollLock"	nil	nil		nil		"SCROLL_LOCK"	"Scroll"	nil	nil	nil	"ScrollLock"	]
    ["<pause>"		"Pause"		119	"Pause"		nil	nil		nil		"PAUSE"		"Pause"		nil	nil	nil	"Pause"		]

    ["`"		"grave"		43	"`"		?`	"~"		"asciitilde"	"GRAVE_ACCENT"	"Grave"		"~"	?~	nil	"`"		]
    ["1"		"1"		2	"1"		?1	"!"		"exclam"	"1"		"Key1"		"!"	?!	nil	"1"		]
    ["2"		"2"		3	"2"		?2	"@"		"at"		"2"		"Key2"		"@"	?@	nil	"2"		]
    ["3"		"3"		4	"3"		?3	"#"		"numbersign"	"3"		"Key3"		"#"	?#	nil	"3"		]
    ["4"		"4"		5	"4"		?4	"$"		"dollar"	"4"		"Key4"		"$"	?$	nil	"4"		]
    ["5"		"5"		6	"5"		?5	"%"		"percent"	"5"		"Key5"		"%"	?%	nil	"5"		]
    ["6"		"6"		7	"6"		?6	"^"		"asciicircum"	"6"		"Key6"		"^"	?^	nil	"6"		]
    ["7"		"7"		8	"7"		?7	"&"		"ampersand"	"7"		"Key7"		"&"	?&	nil	"7"		]
    ["8"		"8"		9	"8"		?8	"*"		"asterisk"	"8"		"Key8"		"*"	?*	nil	"8"		]
    ["9"		"9"		10	"9"		?9	"("		"parenleft"	"9"		"Key9"		"("	?(	nil	"9"		]
    ["0"		"0"		11	"0"		?0	")"		"parenright"	"0"		"Key0"		")"	?)	nil	"0"		]
    ["-"		"minus"		12	"-"		?-	"_"		"underscore"	"MINUS"		"Minus"		"_"	?_	nil	"-"		]
    ["="		"equal"		13	"="		?=	"+"		"plus"		"EQUAL"		"Equals"	"+"	?+	nil	"="		]
    ["<backspace>"	"BackSpace"	14	"Backspace"	#x007F	nil		nil		"BACKSPACE"	"Back"		nil	nil	nil	"Backspace"	]
    ["<tab>"		"Tab"		15	"Tab"		#x0009	"<backtab>"	"ISO_Left_Tab"	"TAB"		"Tab"		nil	nil	nil	"Tab"		]
    ["q"		"q"		16	"Q"		?q	"Q"		"Q"		"Q"		"Q"		nil	?Q	#x0011	"Q"		]
    ["w"		"w"		17	"W"		?w	"W"		"W"		"W"		"W"		nil	?W	#x0017	"W"		]
    ["e"		"e"		18	"E"		?e	"E"		"E"		"E"		"E"		nil	?E	#x0005	"E"		]
    ["r"		"r"		19	"R"		?r	"R"		"R"		"R"		"R"		nil	?R	#x0012	"R"		]
    ["t"		"t"		20	"T"		?t	"T"		"T"		"T"		"T"		nil	?T	#x0014	"T"		]
    ["y"		"y"		21	"Y"		?y	"Y"		"Y"		"Y"		"Y"		nil	?Y	#x0019	"Y"		]
    ["u"		"u"		22	"U"		?u	"U"		"U"		"U"		"U"		nil	?U	#x0015	"U"		]
    ["i"		"i"		23	"I"		?i	"I"		"I"		"I"		"I"		nil	?I	#x0009	"I"		]
    ["o"		"o"		24	"O"		?o	"O"		"O"		"O"		"O"		nil	?O	#x000F	"O"		]
    ["p"		"p"		25	"P"		?p	"P"		"P"		"P"		"P"		nil	?P	#x0010	"P"		]
    ["["		"bracketleft"	26	"["		?\[	"{"		"braceleft"	"LEFT_BRACKET"	"LBracket"	"{"	?{	#x001B	"["		]
    ["]"		"bracketright"	27	"]"		?\]	"}"		"braceright"	"RIGHT_BRACKET"	"RBracket"	"}"	?}	#x001D	"]"		]
    ["<return>"		"Return"	28	"Return"	#x000D	nil		nil		"ENTER"		"Return"	nil	nil	nil	"Enter"		]
    ["<Caps_Lock>"	"Caps_Lock"	58	"CapsLock"	nil	nil		nil		"CAPS_LOCK"	"Capital"	nil	nil	nil	"CapsLock"	]
    ["a"		"a"		30	"A"		?a	"A"		"A"		"A"		"A"		nil	?A	#x0001	"A"		]
    ["s"		"s"		31	"S"		?s	"S"		"S"		"S"		"S"		nil	?S	#x0013	"S"		]
    ["d"		"d"		32	"D"		?d	"D"		"D"		"D"		"D"		nil	?D	#x0004	"D"		]
    ["f"		"f"		33	"F"		?f	"F"		"F"		"F"		"F"		nil	?F	#x0006	"F"		]
    ["g"		"g"		34	"G"		?g	"G"		"G"		"G"		"G"		nil	?G	#x0007	"G"		]
    ["h"		"h"		35	"H"		?h	"H"		"H"		"H"		"H"		nil	?H	#x0008	"H"		]
    ["j"		"j"		36	"J"		?j	"J"		"J"		"J"		"J"		nil	?J	#x000A	"J"		]
    ["k"		"k"		37	"K"		?k	"K"		"K"		"K"		"K"		nil	?K	#x000B	"K"		]
    ["l"		"l"		38	"L"		?l	"L"		"L"		"L"		"L"		nil	?L	#x000C	"L"		]
    [";"		"semicolon"	39	";"		?\;	":"		"colon"		"SEMICOLON"	"Semicolon"	":"	?:	nil	";"		]
    ["'"		"apostrophe"	40	"'"		?'	"\""		"quotedbl"	"APOSTROPHE"	"Apostrophe"	"\""	?\"	nil	"'"		]
    [nil		"Shift_L"	42	"Shift"		nil	nil		nil		"LEFT_SHIFT"	"LShift"	nil	nil	nil	"LeftShift"	]
    ["\\"		"backslash"	43	"\\"		?\\	"|"		"bar"		"BACKSLASH"	"Backslash"	"|"	?|	#x001C	"\\\\"		]
    ["z"		"z"		44	"Z"		?z	"Z"		"Z"		"Z"		"Z"		nil	?Z	#x001A	"Z"		]
    ["x"		"x"		45	"X"		?x	"X"		"X"		"X"		"X"		nil	?X	#x0018	"X"		]
    ["c"		"c"		46	"C"		?c	"C"		"C"		"C"		"C"		nil	?C	#x0003	"C"		]
    ["v"		"v"		47	"V"		?v	"V"		"V"		"V"		"V"		nil	?V	#x0016	"V"		]
    ["b"		"b"		48	"B"		?b	"B"		"B"		"B"		"B"		nil	?B	#x0002	"B"		]
    ["n"		"n"		49	"N"		?n	"N"		"N"		"N"		"N"		nil	?N	#x000E	"N"		]
    ["m"		"m"		50	"M"		?m	"M"		"M"		"M"		"M"		nil	?M	#x000D	"M"		]
    [","		"comma"		51	","		?,	"<"		"less"		"COMMA"		"Comma"		"<"	?<	nil	","		]
    ["."		"period"	52	"."		?.	">"		"greater"	"PERIOD"	"Period"	">"	?>	nil	"."		]
    ["/"		"slash"		53	"/"		?/	"?"		"question"	"SLASH"		"Slash"		"?"	??	nil	"/"		]
    [nil		"Shift_R"	54	"Shift"		nil	nil		nil		"RIGHT_SHIFT"	"RShift"	nil	nil	nil	"RightShift"	]
    [nil		"Control_L"	29	"Ctrl"		nil	nil		nil		"LEFT_CONTROL"	"LControl"	nil	nil	nil	"LeftControl"	]
    [nil		"Super_L"	125	"Meta"		nil	nil		nil		"LEFT_SUPER"	"LWin"		nil	nil	nil	"LeftWindows"	]
    [nil		"Alt_L"		56	"Alt"		nil	nil		nil		"LEFT_ALT"	"LAlt"		nil	nil	nil	"LeftAlt"	]
    ["SPC"		"space"		57	"Space"		#x0020	nil		nil		"SPACE"		"Space"		nil	nil	#x0000	"Space"		]
    [nil		"Alt_R"		100	"Alt"		nil	nil		nil		"RIGHT_ALT"	"RAlt"		nil	nil	nil	"RightAlt"	]
    [nil		"Super_R"	126	"Meta"		nil	nil		nil		"RIGHT_SUPER"	"RWin"		nil	nil	nil	"RightWindows"	]
    ["<menu>"		"Menu"		127	"Menu"		#x0010	nil		nil		"MENU"		"Apps"		nil	nil	nil	"Applications"	]
    [nil		"Control_R"	97	"Ctrl"		nil	nil		nil		"RIGHT_CONTROL"	"RControl"	nil	nil	nil	"RightControl"	]

    ["<up>"		"Up"		103	"Up"		#xF700	nil		nil		"UP"		"Up"		nil	nil	nil	"UpArrow"	]
    ["<down>"		"Down"		108	"Down"		#xF701	nil		nil		"DOWN"		"Down"		nil	nil	nil	"DownArrow"	]
    ["<left>"		"Left"		105	"Left"		#xF702	nil		nil		"LEFT"		"Left"		nil	nil	nil	"LeftArrow"	]
    ["<right>"		"Right"		106	"Right"		#xF703	nil		nil		"RIGHT"		"Right"		nil	nil	nil	"RightArrow"	]

    ["<insert>"		"Insert"	110	"Ins"		#xF746	nil		nil		"INSERT"	"Insert"	nil	nil	nil	"Insert"	]
    ["<delete>"		"Delete"	111	"Del"		#xF728	nil		nil		"DELETE"	"Delete"	nil	nil	nil	"Delete"	]
    ["<home>"		"Home"		102	"Home"		#xF729	nil		nil		"HOME"		"Home"		nil	nil	nil	"Home"		]
    ["<end>"		"End"		107	"End"		#xF72B	nil		nil		"END"		"End"		nil	nil	nil	"End"		]
    ["<prior>"		"Prior"		104	"PgUp"		#xF72C	nil		nil		"PAGE_UP"	"PageUp"	nil	nil	nil	"PageUp"	]
    ["<next>"		"Next"		109	"PgDown"	#xF72D	nil		nil		"PAGE_DOWN"	"PageDown"	nil	nil	nil	"PageDown"	]

    ;; Add new entries at the end of the list, to avoid disrupting
    ;; existing configurations.

    ;; TODO: numpad
    )
  "List of keys supported by the `term-keys' package.

Each item in the list is a 12-element vector:

0: The Emacs key name, as it occurs in `describe-key' or `kbd'.
   nil can be used to indicate keys which Emacs currently does
   not recognize (but are still known by other input systems),
   such as modifier keys (which Emacs can't process on its own,
   only in combination with a non-modifier key).

1: The X11 KeySym name, as returned by XKeysymToString.  Used for
   urxvt/xterm configuration.

2: The keynumber (keycode) from the Linux TTY.  You can obtain a
   key's keynumber by running the 'showkey' program in a TTY.

3: The Qt key name, as returned by QKeySequence::toString and
   accepted by QKeySequence::fromString.  An easy way to obtain
   their name is using any KDE application's \"Configure
   Shortcuts\" dialog.

4: The Unicode character code emitted by the key on macOS.  The
   program \"Key Codes\" by developer \"Many Tricks\" (available
   on the OS App Store) can display these values.

5: The shifted Emacs key name (i.e. the name when the same key
   is pressed while holding Shift), if it is different from the
   base name (index 1); otherwise, nil.  Assumes a standard US
   ASCII layout.

6: The shifted X11 KeySym name (i.e. the name when the same key
   is pressed while holding Shift), if it is different from the
   base name (index 2); otherwise, nil.  Assumes a standard US
   ASCII layout.

7: The GLFW key name, without the \"GLFW_KEY_\" prefix.
   A list can be found here:
   https://www.glfw.org/docs/3.3/group__keys.html

8: The key name as it appears in the Rust winit crate's
   VirtualKeyCode enumeration:
   https://docs.rs/glutin/0.28.0/glutin/event/enum.VirtualKeyCode.html

9: The shifted Qt key name (i.e. the name when the same key
   is pressed while holding Shift), if it is different from the
   base name (index 3); otherwise, nil.  Assumes a standard US
   ASCII layout.

10: The shifted macOS key code (i.e. the code when the same key
   is pressed while holding Shift), if it is different from the
   base code (index 4); otherwise, nil.  Assumes a standard US
   ASCII layout.

11: The control macOS key code (i.e. the code when the same key
   is pressed while holding Ctrl), if it is different from the
   base code (index 4); otherwise, nil.  Assumes a standard US
   ASCII layout.  Has precedence over index 10.

12: The wezterm key identifiers as described at:
   https://wezfurlong.org/wezterm/config/keys.html"
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
	    :tag "Qt key name")
	   (choice
	    :tag "macOS Unicode character code"
	    (const
	     :tag "None"
	     nil)
	    (integer
	     :tag "Character code"))
	   (choice
	    :tag "Shifted X11 KeySym"
	    (const
	     :tag "Same as non-shifted"
	     nil)
	    (string
	     :tag "Shifted key name"))
	   (string
	    :tag "GLFW key name")
	   (string
	    :tag "winit key name")
	   (choice
	    :tag "Shifted Qt key name"
	    (const
	     :tag "Same as non-shifted"
	     nil)
	    (string
	     :tag "Shifted key name"))
	   (choice
	    :tag "Shifted macOS Unicode character code"
	    (const
	     :tag "Same as non-shifted"
	     nil)
	    (integer
	     :tag "Shifted character code"))
	   (choice
	    :tag "Control macOS Unicode character code"
	    (const
	     :tag "Same as without Control"
	     nil)
	    (integer
	     :tag "Control character code"))
	   (string
	    :tag "Wezterm key name")))
  :group 'term-keys)


(defcustom term-keys/prefix "\033\037"
  "Key sequence prefix.

Indicates the byte string to be sent before a term-keys key code.

The default value is \\033\\037 (0x1B 0x1F, or ^[^_).

The prefix, or any starting substring of it, or any sequence
beginning with it, should not be already bound to an action in
Emacs.  E.g. with the default, neither ^[, ^[^_, or ^[^_abc
should by themselves be bound to an Emacs action."
  :type 'string
  :group 'term-keys)


(defcustom term-keys/suffix "\037"
  "Key sequence suffix.

Indicates the end of the data encoding the pressed key
combination.  Can be any character which isn't used in the
`term-keys/encode-number' encoding scheme."
  :type 'string
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

      ;; Navigation keys and Control/Alt
      (and (member key '("Up" "Down" "Left" "Right" "Home" "End" "Prior" "Next")) (or meta (and control shift)))

      ;; S-PgUp/PgDn - usually used for scrolling the terminal, which is not useful in Emacs
      (and (member key '("Prior" "Next")) shift)

      ;; C-S-x is unrepresentable for letters
      (and (string-match-p "^[a-z]$" key) control shift)

      ;; C-x is unrepresentable for digits
      (and (string-match-p "^[0-9]$" key) control)

      ;; ...as well as punctuation and some special characters
      (and (member key '("Return" "Tab" "BackSpace"
			 "grave" "minus" "equal" "bracketleft" "bracketright" "semicolon"
			 "apostrophe" "backslash" "comma" "period" "slash" "space"))
	   control)

      ;; Shift + special chars
      (and (member key '("Return" "BackSpace")) shift)

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


(defun term-keys/format-key (keymap mods)
  "Format key modifiers in Emacs syntax.

Returns key (given in KEYMAP, a `term-keys/mapping' row)
prepended with S-, C-, M-, s-, H-, or A- depending on the
elements of the bool vector MODS are correspondingly non-nil."
  (concat
   (cl-loop for modflag across mods
	    for index from 0
	    if modflag
	    concat (concat (string (elt term-keys/modifier-chars index))
			   "-"))

   ;; Perform Shift-translation
   ;; TODO: we probably should remove the "S-" prefix, but it doesn't
   ;; seem to matter in practice.
   (or
    (and (elt mods 0) (elt keymap 5))
    (elt keymap 0))))

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
    ;; Iterate from 0 to 2^6-1 for a bitmask of all modifier combinations
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

  ;; Hack for Emacs 28 and higher.
  ;; TODO: We want to remove this binding only from the input decode
  ;; step - it should still be accessible via the term-keys protocol.
  (let ((prefix-bind (key-binding term-keys/prefix)))
    (when prefix-bind
      (message "term-keys: term-keys/prefix (%S) is already bound to %s (as %s) - unbinding"
               term-keys/prefix
               prefix-bind
               (key-description term-keys/prefix))
      (global-unset-key term-keys/prefix)))

  (term-keys/iterate-keys
   (lambda (index keymap mods)
     (define-key
       input-decode-map
       (concat
	term-keys/prefix
	(term-keys/encode-key index mods)
	term-keys/suffix)
       (kbd (term-keys/format-key keymap mods))))))


;;;###autoload
(define-minor-mode term-keys-mode
  "`term-keys' global minor mode.

When enabled, automatically set up configured keys for new frames
on TTY terminals.  If the current frame is on a TTY, set it up as
well."
  :global t
  :require 'term-keys
  (if term-keys-mode
      (progn
	(add-hook 'tty-setup-hook 'term-keys/init)
	(if (eq (framep-on-display) t)
	    (term-keys/init)))
    (remove-hook 'tty-setup-hook 'term-keys/init)))


(defconst term-keys/main-file-name (or load-file-name buffer-file-name)
  "Path to this file.  Used for interop.")


(provide 'term-keys)
;;; term-keys.el ends here
