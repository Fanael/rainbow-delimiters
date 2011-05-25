
rainbow-delimiters.el:
======================

Rainbow-delimiters is a "rainbow parentheses"-like Emacs mode for coloring parentheses, brackets, and braces according to their nested depth. It gives a visual reference for which delimiters match, and what statements are at the same depth - if several statements are at the same level, they will be the same color.

This is the official github repository for rainbow-delimiters. It is made available so people can easily contribute to the mode.

The latest version of rainbow-delimiters.el is always found at the following location on EmacsWiki:
http://www.emacswiki.org/emacs/download/rainbow-delimiters.el

Further information is available on the EmacsWiki page:
http://www.emacswiki.org/emacs/RainbowDelimiters



Installation instructions:
--------------------------

1. Place rainbow-delimiters.el on your emacs load-path.

2. Compile the file (necessary for speed):
    <code>M-x byte-compile-file <location of rainbow-delimiters.el></code>
3. Add the following to your dot-emacs/init file:
    <code>(require 'rainbow-delimiters)</code>
4. Add hooks for modes where you want it enabled, for example:
    <code>(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)</code>
- To activate rainbow-delimiters mode temporarily in a buffer:
    <code>M-x rainbow-delimiters-mode</code>
5. When using a dark background, if delimiter colors seem washed out
you may need to add the following to your dot-emacs and restart:
    <code>(setq-default 'frame-background-mode 'dark)</code>

This is because Emacs can guess frame-background-mode incorrectly,
causing rainbow-delimiters to use its light color scheme on dark
backgrounds.

The light/dark color schemes differ only in their brightness level.


Additional discussion is at the top of the file and on the EmacsWiki page linked to above.
