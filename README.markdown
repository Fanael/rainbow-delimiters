
rainbow-delimiters.el:
======================

RainbowDelimiters is a “rainbow parentheses”-like mode which highlights parentheses, brackets, and braces according to their depth. Each successive level is highlighted in a different color. This makes it easy to spot matching delimiters, orient yourself in the code, and tell which statements are at a given depth.

Great care has been taken to make this mode FAST. You shouldn’t see any change in scrolling or editing speed when it’s on even when working in delimiter-rich languages like Clojure, Lisp and Scheme.

You can customize the colors RainbowDelimiters uses. The default colors are intentionally subtle; they are unobtrusive enough to make the mode worth looking at even if you usually don’t like rainbow parentheses modes.

This is the official github repository for rainbow-delimiters. It is made available so people can easily contribute to the mode.

The latest <b>release</b> of rainbow-delimiters.el is always found at the following location on EmacsWiki:
<br />
<b>
* [rainbow-delimiters.el](http://www.emacswiki.org/emacs/download/rainbow-delimiters.el)
</b>

Further information is available on the [EmacsWiki page](http://www.emacswiki.org/emacs/RainbowDelimiters):
<br />
* http://www.emacswiki.org/emacs/RainbowDelimiters



Installation instructions:
--------------------------

1. Place rainbow-delimiters.el on your emacs load-path.

2. Compile the file (necessary for speed):
<br /><code>M-x byte-compile-file <location of rainbow-delimiters.el></code>
3. Add the following to your dot-emacs/init file:
<br /><code>(require 'rainbow-delimiters)</code>
4. Activate the mode in your init file.
<br />
You can choose to enable it only in certain modes, or Emacs-wide:


* To enable it only in specific modes, add lines like the following:
<br /><code>(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)</code>


* To activate the mode globally, add to your init file:
<br /><code>(global-rainbow-delimiters-mode)</code>


* To temporarily activate rainbow-delimiters mode in an open buffer:
<br /><code>M-x rainbow-delimiters-mode</code>

Additional discussion is at the top of the file and on the EmacsWiki page linked to above.
