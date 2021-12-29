# term-keys - lossless keyboard input for Emacs

This package allows configuring Emacs and a supported terminal emulator to handle keyboard input involving any combination of keys and modifiers.

## Table of Contents

  * [Introduction](#introduction)
  * [Installation](#installation)
  * [Setup](#setup)
    * [Configuring Emacs](#configuring-emacs)
    * [Configuring `term-keys`](#configuring-term-keys)
    * [Terminal Emulators](#terminal-emulators)
      * [urxvt (rxvt-unicode)](#urxvt-rxvt-unicode)
      * [xterm](#xterm)
      * [kitty](#kitty)
      * [Alacritty](#alacritty)
      * [Konsole](#konsole)
      * [Yakuake](#yakuake)
      * [Linux console](#linux-console)
      * [st](#st)
      * [macOS Terminal](#macos-terminal)
      * [Unsupported terminals](#unsupported-terminals)
  * [Similar projects](#similar-projects)

## Introduction

Generally, terminal emulators and applications running in a terminal cannot reliably transmit and receive certain keystrokes (keys in combination with modifiers).
For some key combinations, there is no consensus on how these events should be encoded on the wire (F-keys and movement keys plus modifiers);
some other key combinations cannot be represented at all (such as <kbd>Ctrl</kbd><kbd>1</kbd> or <kbd>Ctrl</kbd><kbd>Shift</kbd><kbd>A</kbd>).
This can be an impediment for Emacs users, especially when you've already configured your Emacs in an environment unrestricted by these limitations (X11) and now wish to use it in the terminal as well.

The `term-keys` package allows solving this problem by giving these key combinations a custom, unique encoding, overcoming the limitations of the protocol implemented by the terminal emulators.

## Installation

To install `term-keys`, first add my ELPA package archive. Add this to your `init.el` and evaluate it:

```elisp
(require 'package) ; you may already have this line
(add-to-list 'package-archives
             '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
```

Then, install it like any ELPA package (<kbd>M-x</kbd>`package-install`<kbd>RET</kbd>`term-keys`).

## Setup

Setting up `term-keys` is a three-part process:

1. Configure Emacs;
2. Configure `term-keys` (optional);
3. Configure your terminal emulator.

See the sections below for more information.

### Configuring Emacs

Setting up `term-keys` is as easy as:

```elisp
(require 'term-keys)
(term-keys-mode t)
```

This will automatically set up current and new TTY frames to decode `term-keys` key sequences.
If you prefer to enable it for each frame manually, you can do so by invoking `(term-keys/init)`.

### Configuring `term-keys`

`term-keys` is very configurable. Most things can be changed via Emacs' `customize` interface - use <kbd>M-x</kbd>`customize-group`<kbd>RET</kbd>`term-keys` to access it.

Some of the supplementary code for terminal emulator configuration can be configured as well.
Because it's not loaded by default (as it's only necessary for initial configuration), you need to load it explicitly before configuring it, e.g. using <kbd>M-:</kbd>`(require 'term-keys-konsole)`, and only then invoking Emacs' `customize` interface.
See the terminal's section in this file for more information.

The only part of `term-keys` which cannot be configured via the `customize` interface is the set of key combinations to support, as (counting all possible keys plus combinations of modifier keys) their total number is very large.
For this purpose, `term-keys` allows specifying the name of a function to invoke, which shall implement this logic.
See the documentation of `term-keys/want-key-p-func` and `term-keys/want-key-p-def` (as well as the definition of the latter) for more information.

### Terminal Emulators

Each terminal emulator has its own method of configuration. Consult the section corresponding to your terminal emulator of choice below.

Note that you will need to update the terminal emulator configuration every time you change the `term-keys` configuration.

#### urxvt (rxvt-unicode)

There's two ways to configure `urxvt`: via command-line parameters or X resources.
Command-line configuration consists in adding the key bindings to the `urxvt` invocation directly.
You can use `term-keys/urxvt-script` to create a shell script in this manner:

```elisp
(require 'term-keys-urxvt)
(with-temp-buffer
  (insert (term-keys/urxvt-script))
  (write-region (point-min) (point-max) "~/launch-urxvt-with-term-keys.sh"))
```

Afterwards, you can run e.g.:

```bash
$ sh ~/launch-urxvt-with-term-keys.sh -e emacs -nw
```

This will launch Emacs under an `urxvt` instance configured for `term-keys`.

X resource configuration consists in adding the `term-keys` configuration to the X resources.
The X resources are global (per X session), and will apply to all newly-started `urxvt` instances.
You can use `term-keys/urxvt-xresources` to create the necessary configuration in this manner:

```elisp
(require 'term-keys-urxvt)
(with-temp-buffer
  (insert (term-keys/urxvt-xresources))
  (append-to-file (point-min) (point-max) "~/.Xresources"))
```

Then use `xrdb` to load the file into memory:

```bash
$ xrdb -merge ~/.Xresources
```

In addition to generating a static configuration as a shell script or X resources, you can ask `term-keys` to invoke `urxvt` directly.
This has the benefit that it will always use the up-to-date `term-keys` configuration, but the downside that it must be done by invoking `emacs` or `emacsclient`.
For example:

```bash
$ emacsclient --eval '(term-keys/urxvt-run-emacs)'
```

See the `term-keys/urxvt-*` definitions for more urxvt-specific help code.

You may also want to disable some default `urxvt` shortcuts which may interfere with using Emacs.
Consider adding something like this to your `~/.Xresources`:

```
URxvt*iso14755: 0
URxvt.keysym.Shift-Insert: builtin-string:
URxvt.keysym.M-S: builtin-string:
```

#### xterm

`xterm` is configured nearly identically as `urxvt`;
thus, this section will be very similar to the `urxvt` section above.

Note: `xterm` supports an extended input mode; see `modifyOtherKeys` et al in the `xterm(1)` man page.
You may find it sufficient for your use case even without `term-keys`.

As with `urxvt`, there's two ways to configure `xterm`: via command-line parameters or X resources.
Command-line configuration consists in adding the key bindings to the `xterm` invocation directly.
You can use `term-keys/xterm-script` to create a shell script in this manner:

```elisp
(require 'term-keys-xterm)
(with-temp-buffer
  (insert (term-keys/xterm-script))
  (write-region (point-min) (point-max) "~/launch-xterm-with-term-keys.sh"))
```

Afterwards, you can run e.g.:

```bash
$ sh ~/launch-xterm-with-term-keys.sh -e emacs -nw
```

This will launch Emacs under an `xterm` instance configured for `term-keys`.

X resource configuration consists in adding the `term-keys` configuration to the X resources.
The X resources are global (per X session), and will apply to all newly-started `xterm` instances.
You can use `term-keys/xterm-xresources` to create the necessary configuration in this manner:

```elisp
(require 'term-keys-xterm)
(with-temp-buffer
  (insert (term-keys/xterm-xresources))
  (append-to-file (point-min) (point-max) "~/.Xresources"))
```

Then use `xrdb` to load the file into memory:

```bash
$ xrdb -merge ~/.Xresources
```

In addition to generating a static configuration as a shell script or X resources, you can ask `term-keys` to invoke `xterm` directly.
This has the benefit that it will always use the up-to-date `term-keys` configuration, but the downside that it must be done by invoking `emacs` or `emacsclient`.
For example:

```bash
$ emacsclient --eval '(term-keys/xterm-run-emacs)'
```

See the `term-keys/xterm-*` definitions for more xterm-specific help code.

You may also want to disable the `eightBitInput` `xterm` option, e.g. with `-xrm 'XTerm*eightBitInput: false'`.

#### kitty

[kitty](https://sw.kovidgoyal.net/kitty/) is configured via its `kitty.conf` configuration file.

To configure kitty for `term-keys`, use `term-keys/kitty-conf` to generate a `kitty.conf` fragment:

```elisp
(require 'term-keys-kitty)
(with-temp-buffer
  (insert (term-keys/kitty-conf))
  (write-region (point-min) (point-max) "~/kitty-for-term-keys.conf"))
```

Then, add the output to your main `kitty.conf` file.

You can customize kitty's mapping of GLFW modifiers to Emacs modifiers in the generated configuration using the respective `customize` group, i.e.: <kbd>M-:</kbd>`(progn (require 'term-keys-kitty) (customize-group 'term-keys/glfw))`

#### Alacritty

[Alacritty](https://github.com/alacritty/alacritty) is configured via its `alacritty.yml` configuration file.

To configure alacritty for `term-keys`, use `term-keys/alacritty-config` to generate a `alacritty.yml` fragment:

```elisp
(require 'term-keys-alacritty)
(with-temp-buffer
  (insert (term-keys/alacritty-config))
  (write-region (point-min) (point-max) "~/alacritty-for-term-keys.yml"))
```

Then, add the output to your main `alacritty.yml` file.

You can customize Alacritty's mapping of its supported modifiers to Emacs modifiers in the generated configuration using the respective `customize` group, i.e.: <kbd>M-:</kbd>`(progn (require 'term-keys-alacritty) (customize-group 'term-keys/alacritty))`

#### Konsole

Konsole provides an interface for adding new and editing existing escape sequences for key combinations 
(Settings &rarr; Edit current profile... &rarr; Keyboard &rarr; Edit).
`term-keys` can generate a Konsole keyboard profile according to its settings.

To do so:

1. Create a new keyboard profile, based on the default one.  
   (Select "Default (XFree 4)", and click "New...".)  
   Name it e.g. `Emacs`.
2. Append the output of `term-keys/konsole-keytab` to the newly created `.keytab` file, e.g.:

   ```elisp
   (require 'term-keys-konsole)
   (with-temp-buffer
	 (insert (term-keys/konsole-keytab))
	 (append-to-file (point-min) (point-max) "~/.local/share/konsole/Emacs.keytab"))
   ```
   
3. Assign the keyboard profile to a new or existing Konsole profile.

You can customize the mapping of Konsole (Qt) modifiers to Emacs modifiers using the respective `customize` group, i.e.: <kbd>M-:</kbd>`(progn (require 'term-keys-konsole) (customize-group 'term-keys/konsole))`

You may also want to disable some default Konsole shortcuts (Settings &rarr; Configure Shortcuts...),
as they may interfere with standard Emacs commands.

#### Yakuake

Yakuake seems to share much with Konsole, and can be configured in the same way.
See the section above for details.

#### Linux console

The Linux console can be customized using `.keymap` files and the `loadkeys` program.
You can configure it for `term-keys` as follows:

1. Use `term-keys/linux-keymap` to create a `.keymap` file:

   ```elisp
   (require 'term-keys-linux)
   (with-temp-buffer
	 (insert (term-keys/linux-keymap))
	 (write-region (point-min) (point-max) "~/term-keys.keymap"))
   ```

2. Load the created `.keymap` file:

   ```bash
   $ sudo loadkeys ~/term-keys.keymap
   ```

To reset the layout to the default one, run `sudo loadkeys -d`.

You will need to invoke `loadkeys` after every boot; alternatively, on
systemd distributions, you can add a `KEYMAP=` line to
`/etc/vconsole.conf`.

You can customize some settings affecting the generated `.keymap` files using the respective `customize` group, i.e.: <kbd>M-:</kbd>`(progn (require 'term-keys-linux) (customize-group 'term-keys/linux))`

Note that the `.keymap` files generated by `term-keys/linux-keymap` assume a QWERTY keyboard layout.
If you use another layout (AZERTY, Dvorak, Colemak...), you will need to customize `term-keys/mapping` and set the alphanumeric keynumbers accordingly.

Note also that Linux has a limitation on the number of customized keys. Custom key strings need to be assigned to "function keys", for which there are 256 slots by default (which includes the default strings for the F-keys and some other keys like Insert/Delete). This leaves about 234 slots for `term-keys`, which is sufficient for the default configuration, but may not be sufficient for a custom one, especially if you enable support for additional modifiers.

#### st

[st](https://st.suckless.org/) is configured by editing its `config.h` and recompiling. The key sequences can be configured in this way as well.

You can configure st for `term-keys` as follows:

1. Generate the initial `config.h`, e.g. by building `st` once.
2. Use the `term-keys/st-config-*` functions to create header files:

   ```elisp
   (require 'term-keys-st)
   ;; Assuming st is checked out in ~/st
   (with-temp-buffer
	 (insert (term-keys/st-config-key))
	 (write-region (point-min) (point-max) "~/st/config-term-keys-key.h"))
   (with-temp-buffer
	 (insert (term-keys/st-config-mappedkeys))
	 (write-region (point-min) (point-max) "~/st/config-term-keys-mappedkeys.h"))
   ```

3. Update the definition of `mappedkeys` in `config.h` as follows:

   ```c
   static KeySym mappedkeys[] = {
   #include "config-term-keys-mappedkeys.h"
	   -1
   };
   ```

4. Update the definition of `key` in `config.h` as follows:

   ```c
   static Key key[] = {
   #include "config-term-keys-key.h"
	   /* keysym           mask            string      appkey appcursor crlf */
	   /* ( ... original definitions follow ... ) */
   ```

5. Rebuild st.

You can customize st's mapping of X11 modifiers to Emacs modifiers in the generated configuration using the respective `customize` group, i.e.: <kbd>M-:</kbd>`(progn (require 'term-keys-st) (customize-group 'term-keys/x11))`

#### macOS Terminal

The standard macOS Terminal.app allows, to a certain extent, customizing escape sequences sent by key combinations
(Terminal &rarr; Preferences... &rarr; Profiles &rarr; (select a profile) &rarr; Keyboard).
`term-keys` can generate a configuration XML file, which can be injected into Terminal.app's preferences plist file.

To do so:

1. Create a new profile. Name it e.g. `Emacs`.
2. Enable the "Use Option as Meta key" option on the Keyboard tab.
3. Export the `term-keys` keymap XML file:

   ```elisp
   (require 'term-keys-terminal-app)
   (with-temp-buffer
     (insert (term-keys/terminal-app-keymap-xml))
     (append-to-file (point-min) (point-max) "~/term-keys.xml"))
   ```

4. Import the keymap into Terminal.app's preferences file:

   ```bash
   $ plutil -replace 'Window Settings.Emacs.keyMapBoundKeys' \
       -xml "$(cat ~/term-keys.xml)" \
       ~/Library/Preferences/com.apple.Terminal.plist
   ```

   (If you named your profile something other than `Emacs`, substitute its name in the command above.)

5. Restart Terminal.app.

Note that the application's settings UI only allows configuring a small set of keys.
This limitation does not carry over to the underlying configuration file format, and thus does not apply to `term-keys` -
the generated keymap file will contain definitions for all keys that generate a Unicode key code on macOS.
These entries will appear (displayed with the key's hex code instead of the key name) in the terminal emulator's configuration, but cannot be edited there.

You can customize the mapping of Terminal.app modifiers to Emacs modifiers using the respective `customize` group, i.e.: <kbd>M-:</kbd>`(progn (require 'term-keys-terminal-app) (customize-group 'term-keys/terminal-app))`

#### Unsupported terminals

These terminals don't (directly) support customizing key bindings, and thus cannot be used with `term-keys`:

- **PuTTY** - Custom key mappings are [a known frequently-requested feature](https://www.chiark.greenend.org.uk/~sgtatham/putty/wishlist/key-mapping.html).
- **GNOME Terminal** - Uses the [Gnome VTE widget](https://github.com/GNOME/vte), which doesn't seem to support key binding customization.
- **tilix**, **Guake**, **tilda** - Use the same widget as GNOME Terminal.
- **GNOME Terminator** - Uses the same widget as GNOME Terminal, however has a plugin system. Perhaps writing a plugin is possible?
- **termite** - Uses the same widget as GNOME Terminal, however, it has a `modify_other_keys` option, similar to xterm's `modifyOtherKeys`.
- **mintty** - No support for customizing key bindings, however, it already [has some support](https://github.com/mintty/mintty/wiki/CtrlSeqs) for xterm's `modifyOtherKeys` protocol extensions.
- **Bitvise SSH Client** - No support for customizing key bindings. Supports a custom extended protocol (bvterm), which seems to allow lossless keyboard input (including key-up events), however it is full of Windows-isms (much of the protocol uses the same data types as used by the Windows API), and getting Emacs to speak it would be a non-trivial task that's not likely to be achievable using just Emacs Lisp code.
- **Terminology** - No support for customizing key bindings at runtime (via a configuration file), however, this terminal generates its key/string translation tables using another terminal emulator (i.e. by sending keyboard events with `xdotool` to another terminal emulator and recording the resulting strings). Therefore, it may be possible to configure Terminology with `term-keys` by running its configuration script through another terminal pre-configured with `term-keys` (e.g. `xterm`), then rebuilding Terminology using the resulting configuration.

## Similar projects

* [xterm-keybinder](https://github.com/yuutayamada/xterm-keybinder-el) is an Emacs package similar to this one. `term-keys` improves upon xterm-keybinder by supporting more terminals, better documentation, customizability, and extensibility, and improved wire efficiency.

* [notty](https://github.com/withoutboats/notty) is an experimental terminal emulator which also aims at achieving lossless keyboard input by extending the ANSI protocol. `term-keys` does not use notty's protocol because its escape sequence prefix (`^[{`) conflicts with a default Emacs command binding (`backward-paragraph`).

* [Fix Keyboard Input on Terminals](http://www.leonerd.org.uk/hacks/fixterms/) is a proposal to improve input in terminals, which is to some extent implemented in `xterm` and Emacs. Unfortunately, the proposed protocol is vague and incomplete, as it does not cover modifier keys and many PC keyboard keys.

* The [kitty terminal emulator](https://github.com/kovidgoyal/kitty) (the one written in Python, not [the PuTTY fork](http://www.9bis.net/kitty/)) implements [its own protocol extensions for keyboard handling](https://github.com/kovidgoyal/kitty/blob/master/protocol-extensions.asciidoc#keyboard-handling) (though it can also be simply configured with term-keys as [described above](#kitty)).
