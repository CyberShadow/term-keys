;;; term-keys-wezterm.el --- term-keys support for wezterm

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
;; configuration of the wezterm terminal emulator to interoperate with
;; the term-keys package.

;; For more information, please see the accompanying README.md file.

;;; Code:


(require 'term-keys)
(require 'term-keys-glfw-mods)


(defun term-keys/wezterm-format-key (keymap mods)
  "Format key in wezterm syntax according to mods."
  (or                    ; Apply shift
   (and (elt mods 0)     ; With Shift?
	(elt keymap 13)) ; Use shifted column
   (elt keymap 12)))     ; Use non-shifted column


(defun term-keys/wezterm-format-mods (mods)
  "Format modifiers in wezterm syntax.

Returns the wezterm key combination string corresponding to the
KEYMAP and modifier state MODS."
  (string-join
   (cl-loop for modflag across mods
	    for index from 0
	    if modflag
	    collect
	    (elt term-keys/glfw-modifier-map index))
   "|"))


(defun term-keys/wezterm-conf ()
  "Construct wezterm configuration (wezterm.lua fragment).

This function returns, as a string, the wezterm.lua key map lines
necessary to configure wezterm to encode term-keys key sequences,
according to the term-keys configuration."
  (concat "enable_kitty_keyboard = true,\nkeys = {\n"
          (apply #'concat
	         (term-keys/iterate-keys
	          (lambda (index keymap mods)
	            (format "  {key = \"%s\", mods = \"%s\", action = wezterm.action{SendString=\"%s\"}},\n"
		            (term-keys/wezterm-format-key keymap mods)
		            (term-keys/wezterm-format-mods mods)
		            (mapconcat
		             (lambda (c) (format "\\x%02x" c))
		             (append
		              term-keys/prefix
		              (term-keys/encode-key index mods)
		              term-keys/suffix
		              nil)
		             "")))))
          "},"))


(provide 'term-keys-wezterm)
;;; term-keys-wezterm.el ends here
