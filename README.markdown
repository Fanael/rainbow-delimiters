
rainbow-delimiters.el:
======================

Rainbow-delimiters is a "rainbow parentheses"-like Emacs mode for coloring parentheses, brackets, and braces according to their nested depth. It gives a visual reference for which delimiters match, and what statements are at the same depth - if several statements are at the same level, they will be the same color.

This is the official github repository for rainbow-delimiters. It is made available so people can easily contribute to the mode.

The latest release of rainbow-delimiters.el is always found at the following location on EmacsWiki:
<p>
[rainbow-delimiters.el](http://www.emacswiki.org/emacs/download/rainbow-delimiters.el)
</p>

Further information is available on the [EmacsWiki page](http://www.emacswiki.org/emacs/RainbowDelimiters):
<br />http://www.emacswiki.org/emacs/RainbowDelimiters



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


- To enable it only in specific modes, add lines like the following:
<br /><code>(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)</code>


- To activate the mode globally, add to your init file:
<br /><code>(global-rainbow-delimiters-mode)</code>


- To temporarily activate rainbow-delimiters mode in an open buffer:
<br /><code>M-x rainbow-delimiters-mode</code>

Additional discussion is at the top of the file and on the EmacsWiki page linked to above.
