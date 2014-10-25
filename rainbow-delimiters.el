;;; rainbow-delimiters.el --- Highlight nested parens, brackets, braces a different color at each depth.

;; Copyright (C)
;;   2010-2013 Jeremy Rayman
;;   2013-2014 Fanael Linithien
;; Author: Jeremy Rayman <opensource@jeremyrayman.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; Created: 2010-09-02
;; Version: 1.3.13
;; Keywords: faces, convenience, lisp, tools
;; Homepage: https://github.com/jlr/rainbow-delimiters

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Rainbow-delimiters is a “rainbow parentheses”-like mode which highlights
;; parentheses, brackets, and braces according to their depth. Each
;; successive level is highlighted in a different color. This makes it easy
;; to spot matching delimiters, orient yourself in the code, and tell which
;; statements are at a given level.
;;
;; Great care has been taken to make this mode FAST. You shouldn't see
;; any discernible change in scrolling or editing speed while using it,
;; even in delimiter-rich languages like Clojure, Lisp, and Scheme.
;;
;; Default colors are subtle, with the philosophy that syntax highlighting
;; shouldn't be visually intrusive. Color schemes are always a matter of
;; taste.  If you take the time to design a new color scheme, please share
;; (even a simple list of colors works) on the EmacsWiki page or via github.
;; EmacsWiki: http://www.emacswiki.org/emacs/RainbowDelimiters
;; Github: http://github.com/jlr/rainbow-delimiters


;;; Installation:

;; 1. Place rainbow-delimiters.el on your emacs load-path.
;;
;; 2. Compile the file (necessary for speed):
;; M-x byte-compile-file <location of rainbow-delimiters.el>
;;
;; 3. Add the following to your dot-emacs/init file:
;; (require 'rainbow-delimiters)
;;
;; 4. Activate the mode in your init file.
;;
;; - To enable it only in certain modes, add lines like the following:
;; (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
;;
;; - To enable it in all programming-related emacs modes (Emacs 24+):
;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;
;; - To toggle rainbow-delimiters mode in an open buffer:
;; M-x rainbow-delimiters-mode

;;; Customization:

;; To customize various options, including the color scheme:
;; M-x customize-group rainbow-delimiters
;;
;; deftheme / color-theme.el users:
;; You can specify custom colors by adding the appropriate faces to your theme.
;; - Faces take the form of:
;;   'rainbow-delimiters-depth-#-face' with # being the depth.
;;   Depth begins at 1, the outermost color.
;;   Faces exist for depths 1-9.
;; - The unmatched delimiter face (normally colored red) is:
;;   'rainbow-delimiters-unmatched-face'

;;; TODO:

;; - Add support for independent depth tracking of each delimiter type
;;   for users of C-like languages.
;; - Python style - increase depth with each new indentation.
;; - Add support for nested tags (XML, HTML)
;; - Set up proper example defthemes for rainbow-delimiters faces.
;; - Intelligent support for other languages: Ruby, LaTeX tags, et al.

;;; Code:

;;; Customize interface:

(defgroup rainbow-delimiters nil
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :prefix "rainbow-delimiters-"
  :link '(url-link :tag "Website for rainbow-delimiters"
                   "https://github.com/jlr/rainbow-delimiters")
  :group 'applications)

(defgroup rainbow-delimiters-faces nil
  "Faces for successively nested pairs of delimiters.

When depth exceeds innermost defined face, colors cycle back through."
  :tag "Color Scheme"
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :link '(custom-group-link :tag "Toggle Delimiters" "rainbow-delimiters-toggle-delimiter-highlighting")
  :prefix "rainbow-delimiters-")

;; Choose which delimiters you want to highlight in your preferred language:

(defgroup rainbow-delimiters-toggle-delimiter-highlighting nil
  "Choose which delimiters to highlight."
  :tag "Toggle Delimiters"
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :link '(custom-group-link :tag "Color Scheme" "rainbow-delimiters-faces"))

(defcustom rainbow-delimiters-highlight-parens-p t
  "Enable highlighting of nested parentheses -- ().

Non-nil (default) enables highlighting of parentheses.
Nil disables parentheses highlighting."
  :tag "Highlight Parentheses?"
  :type 'boolean
  :group 'rainbow-delimiters-toggle-delimiter-highlighting)

(defcustom rainbow-delimiters-highlight-brackets-p t
  "Enable highlighting of nested brackets -- [].

Non-nil (default) enables highlighting of brackets.
Nil disables bracket highlighting."
  :tag "Highlight Brackets?"
  :type 'boolean
  :group 'rainbow-delimiters-toggle-delimiter-highlighting)

(defcustom rainbow-delimiters-highlight-braces-p t
  "Enable highlighting of nested braces -- {}.

Non-nil (default) enables highlighting of braces.
Nil disables brace highlighting."
  :tag "Highlight Braces?"
  :type 'boolean
  :group 'rainbow-delimiters-toggle-delimiter-highlighting)


;;; Faces:

;; Unmatched delimiter face:
(defface rainbow-delimiters-unmatched-face
  '((((background light)) (:foreground "#88090B"))
    (((background dark)) (:foreground "#88090B")))
  "Face to highlight unmatched closing delimiters in."
  :group 'rainbow-delimiters-faces)

;; Mismatched delimiter face:
(defface rainbow-delimiters-mismatched-face
  '((t :inherit rainbow-delimiters-unmatched-face))
  "Face to highlight mismatched closing delimiters in."
  :group 'rainbow-delimiters-faces)

;; Faces for highlighting delimiters by nesting level:
(defface rainbow-delimiters-depth-1-face
  '((((background light)) (:foreground "#707183"))
    (((background dark)) (:foreground "grey55")))
  "Nested delimiters face, depth 1 - outermost set."
  :tag "Rainbow Delimiters Depth 1 Face -- OUTERMOST"
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-2-face
  '((((background light)) (:foreground "#7388d6"))
    (((background dark)) (:foreground "#93a8c6")))
  "Nested delimiters face, depth 2."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-3-face
  '((((background light)) (:foreground "#909183"))
    (((background dark)) (:foreground "#b0b1a3")))
  "Nested delimiters face, depth 3."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-4-face
  '((((background light)) (:foreground "#709870"))
    (((background dark)) (:foreground "#97b098")))
  "Nested delimiters face, depth 4."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-5-face
  '((((background light)) (:foreground "#907373"))
    (((background dark)) (:foreground "#aebed8")))
  "Nested delimiters face, depth 5."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-6-face
  '((((background light)) (:foreground "#6276ba"))
    (((background dark)) (:foreground "#b0b0b3")))
  "Nested delimiters face, depth 6."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-7-face
  '((((background light)) (:foreground "#858580"))
    (((background dark)) (:foreground "#90a890")))
  "Nested delimiters face, depth 7."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-8-face
  '((((background light)) (:foreground "#80a880"))
    (((background dark)) (:foreground "#a2b6da")))
  "Nested delimiters face, depth 8."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-9-face
  '((((background light)) (:foreground "#887070"))
    (((background dark)) (:foreground "#9cb6ad")))
  "Nested delimiters face, depth 9."
  :group 'rainbow-delimiters-faces)

;;; Faces 10+:
;; NOTE: Currently unused. Additional faces for depths 10+ can be added on request.

(defconst rainbow-delimiters-max-face-count 9
  "Number of faces defined for highlighting delimiter levels.

Determines depth at which to cycle through faces again.")

(defcustom rainbow-delimiters-outermost-only-face-count 0
  "Number of faces to be used only for N outermost delimiter levels.

This should be smaller than `rainbow-delimiters-max-face-count'."
  :type 'integer
  :group 'rainbow-delimiters-faces)

;;; Face utility functions

(defun rainbow-delimiters--depth-face (depth)
  "Return face name for DEPTH as a symbol 'rainbow-delimiters-depth-DEPTH-face'.

For example: `rainbow-delimiters-depth-1-face'."
  (intern-soft
   (concat "rainbow-delimiters-depth-"
           (number-to-string
            (if (<= depth rainbow-delimiters-max-face-count)
                ;; Our nesting depth has a face defined for it.
                depth
              ;; Deeper than # of defined faces; cycle back through to
              ;; `rainbow-delimiters-outermost-only-face-count' + 1.
              ;; Return face # that corresponds to current nesting level.
              (+ 1 rainbow-delimiters-outermost-only-face-count
                 (mod (- depth rainbow-delimiters-max-face-count 1)
                      (- rainbow-delimiters-max-face-count
                         rainbow-delimiters-outermost-only-face-count)))))
           "-face")))

;;; Parse partial sexp cache

;; If the block inside the delimiters is too big (where "too big" is
;; in some way related to `jit-lock-chunk-size'), `syntax-ppss' will
;; for some reason return wrong depth. Is it because we're misusing
;; it? Is it because it's buggy? Nobody knows. But users do notice it,
;; and have reported it as a bug. Hence this workaround: don't use
;; `syntax-ppss' at all, use the low-level primitive instead. However,
;; naively replacing `syntax-ppss' with `parse-partial-sexp' slows
;; down the delimiter highlighting noticeably in big files. Therefore,
;; we build a simple cache around it. This brings the speed to around
;; what it used to be, while fixing the bug. See issue #25.

(defvar rainbow-delimiters--parse-partial-sexp-cache nil
  "Cache of the last `parse-partial-sexp' call.

It's a list of conses, where car is the position for which `parse-partial-sexp'
was called and cdr is the result of the call.
The list is ordered descending by car.")
(make-variable-buffer-local 'rainbow-delimiters--parse-partial-sexp-cache)

(defconst rainbow-delimiters--parse-partial-sexp-cache-max-span 20000)

(defun rainbow-delimiters--syntax-ppss-flush-cache (beg _end)
  "Flush the `parse-partial-sexp' cache starting from position BEG."
  (let ((it rainbow-delimiters--parse-partial-sexp-cache))
    (while (and it (>= (caar it) beg))
      (setq it (cdr it)))
    (setq rainbow-delimiters--parse-partial-sexp-cache it)))

(defun rainbow-delimiters--syntax-ppss-run (from to oldstate)
  "Run `parse-partial-sexp' from FROM to TO starting with state OLDSTATE.

Intermediate `parse-partial-sexp' results are prepended to the cache."
  (if (= from to)
      (parse-partial-sexp from to nil nil oldstate)
    (while (< from to)
      (let* ((newpos (min to (+ from rainbow-delimiters--parse-partial-sexp-cache-max-span)))
             (state (parse-partial-sexp from newpos nil nil oldstate)))
        (when (/= newpos to)
          (push (cons newpos state) rainbow-delimiters--parse-partial-sexp-cache))
        (setq oldstate state
              from newpos)))
    oldstate))

(defun rainbow-delimiters--syntax-ppss (pos)
  "Parse-Partial-Sexp State at POS, defaulting to point.

The returned value is the same as that of `parse-partial-sexp' from
`point-min' to POS, except that positions 2 and 6 cannot be relied
upon.

This is essentialy `syntax-ppss', only specific to rainbow-delimiters
to work around a bug."
  (save-excursion
    (let ((it rainbow-delimiters--parse-partial-sexp-cache))
      (while (and it (>= (caar it) pos))
        (setq it (cdr it)))
      (let ((nearest-before (if (consp it) (car it) it)))
        (if nearest-before
            (rainbow-delimiters--syntax-ppss-run (car nearest-before) pos (cdr nearest-before))
          (rainbow-delimiters--syntax-ppss-run (point-min) pos nil))))))

;;; Nesting level

(defvar rainbow-delimiters--syntax-table nil
  "Syntax table (inherited from `major-mode''s) which uses all delimiters.

When `rainbow-delimiters-mode' is first activated, it sets this variable and
the other rainbow-delimiters specific syntax tables based on the current
`major-mode'.
The syntax table is constructed by the function
`rainbow-delimiters--make-syntax-table'.")

;; Syntax table: used with `rainbow-delimiters--syntax-ppss' for determining
;; current depth.
(defun rainbow-delimiters--make-syntax-table (syntax-table)
  "Inherit SYNTAX-TABLE and add delimiters intended to be highlighted by mode."
  (let ((table (copy-syntax-table syntax-table)))
    ;; Modify the entries only if the characters are not recognized as
    ;; delimiters. This ensures that if the characters can be a part of
    ;; multicharacter comment delimiter (e.g. {- -} in Haskell), we don't
    ;; suddenly stop recognizing them as comments.
    (when (/= ?\( (char-syntax ?\())
      (modify-syntax-entry ?\( "()" table))
    (when (/= ?\( (char-syntax ?\[))
      (modify-syntax-entry ?\[ "(]" table))
    (when (/= ?\( (char-syntax ?\{))
      (modify-syntax-entry ?\{ "(}" table))
    (when (/= ?\) (char-syntax ?\)))
      (modify-syntax-entry ?\) ")(" table))
    (when (/= ?\) (char-syntax ?\]))
      (modify-syntax-entry ?\) ")[" table))
    (when (/= ?\) (char-syntax ?\}))
      (modify-syntax-entry ?\) "){" table))
    table))

(defun rainbow-delimiters--depth (ppss)
  "Return # of nested levels of delimiters at parse state PPSS."
  (let ((depth (car ppss)))
    (if (>= depth 0)
        depth
      0))) ; Ignore negative depths created by unmatched closing parens.

;;; Text properties

(defun rainbow-delimiters--propertize-delimiter (loc depth match)
  "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.
MATCH is nil iff it's a mismatched closing delimiter."
  (let ((delim-face (cond
                     ((<= depth 0)
                      'rainbow-delimiters-unmatched-face)
                     ((not match)
                      'rainbow-delimiters-mismatched-face)
                     (t
                      (rainbow-delimiters--depth-face depth)))))
    (font-lock-prepend-text-property loc (1+ loc) 'face delim-face)))

(defvar rainbow-delimiters-escaped-char-predicate nil)
(make-variable-buffer-local 'rainbow-delimiters-escaped-char-predicate)

(defvar rainbow-delimiters-escaped-char-predicate-list
  '((emacs-lisp-mode . rainbow-delimiters--escaped-char-predicate-emacs-lisp)
    (lisp-interaction-mode . rainbow-delimiters--escaped-char-predicate-emacs-lisp)
    (inferior-emacs-lisp-mode . rainbow-delimiters--escaped-char-predicate-emacs-lisp)
    (lisp-mode . rainbow-delimiters--escaped-char-predicate-lisp)
    (scheme-mode . rainbow-delimiters--escaped-char-predicate-lisp)
    (clojure-mode . rainbow-delimiters--escaped-char-predicate-lisp)
    (inferior-scheme-mode . rainbow-delimiters--escaped-char-predicate-lisp)
    ))

(defun rainbow-delimiters--escaped-char-predicate-emacs-lisp (loc)
  "Non-nil iff the character at LOC is escaped as per Emacs Lisp rules."
  (or (and (eq (char-before loc) ?\?) ; e.g. ?) - deprecated, but people use it
           (not (and (eq (char-before (1- loc)) ?\\) ; special case: ignore ?\?
                     (eq (char-before (- loc 2)) ?\?))))
      (and (eq (char-before loc) ?\\) ; escaped char, e.g. ?\) - not counted
           (eq (char-before (1- loc)) ?\?))))

(defun rainbow-delimiters--escaped-char-predicate-lisp (loc)
  "Non-nil iff the character at LOC is escaped as per some generic Lisp rules."
  (eq (char-before loc) ?\\))

(defun rainbow-delimiters--char-ineligible-p (loc ppss)
  "Return t if char at LOC should be skipped, e.g. if inside a comment.
PPSS should be the `parse-partial-sexp' state at LOC.

Returns t if char at loc meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
  (or
   (nth 3 ppss)                ; inside string?
   (nth 4 ppss)                ; inside comment?
   (let ((loc-syntax (car (syntax-after loc)))) ; starting a comment?
     ;; Note: no need to consider single-char openers, they're already handled
     ;; by looking at ppss.
     (cond
      ;; Two character opener, LOC at the first character?
      ((/= 0 (logand #x10000 loc-syntax))
       (/= 0 (logand #x20000 (or (car (syntax-after (1+ loc))) 0))))
      ;; Two character opener, LOC at the second character?
      ((/= 0 (logand #x20000 loc-syntax))
       (/= 0 (logand #x10000 (or (car (syntax-after (1- loc))) 0))))
      (t
       nil)))
   (when rainbow-delimiters-escaped-char-predicate
     (funcall rainbow-delimiters-escaped-char-predicate loc))))

(defun rainbow-delimiters--apply-color (delim depth loc match)
  "Apply color for DEPTH to DELIM at LOC following user settings.

DELIM is a symbol of the variable specifying whether to highlight this delimiter
type.
DEPTH is the delimiter depth, or corresponding face # if colors are repeating.
LOC is location of character (delimiter) to be colorized.
MATCH is nil iff it's a mismatched closing delimiter."
  ;; Ensure user has enabled highlighting of this delimiter type.
  (when (symbol-value delim)
    (rainbow-delimiters--propertize-delimiter loc
                                              depth
                                              match)))

;;; Font-Lock functionality

(defconst rainbow-delimiters--delim-regex "[]()[{}]"
  "Regex matching all opening and closing delimiters the mode highlights.")

(defconst rainbow-delimiters--opening-delim-info
  '((?\( . rainbow-delimiters-highlight-parens-p)
    (?\{ . rainbow-delimiters-highlight-braces-p)
    (?\[ . rainbow-delimiters-highlight-brackets-p))
  "Open delimiter information: list of (DELIMITER . TYPE).

DELIMITER is the opening delimiter.
TYPE is the delimiter type for `rainbow-delimiters-apply-color'.")

(defconst rainbow-delimiters--closing-delim-info
  '((?\) ?\( . rainbow-delimiters-highlight-parens-p)
    (?\} ?\{ . rainbow-delimiters-highlight-braces-p)
    (?\] ?\[ . rainbow-delimiters-highlight-brackets-p))
  "Closing delimiter information: list of (DELIMITER OPENING . TYPE).

DELIMITER is the closing delimiter.
OPENING is the corresponding opening delimiter.
TYPE is the delimiter type for `rainbow-delimiters-apply-color'.")

;; Main function called by font-lock.
(defun rainbow-delimiters--propertize (end)
  "Highlight delimiters in region between point and END.

Used by font-lock for dynamic highlighting."
  (setq rainbow-delimiters-escaped-char-predicate
        (cdr (assoc major-mode rainbow-delimiters-escaped-char-predicate-list)))
  (when rainbow-delimiters--syntax-table
    (with-syntax-table rainbow-delimiters--syntax-table
      (let ((inhibit-point-motion-hooks t))
        ;; Point can be anywhere in buffer; determine the nesting depth at point.
        (let* ((last-ppss-pos (point))
               (ppss (rainbow-delimiters--syntax-ppss last-ppss-pos))
               (depth (rainbow-delimiters--depth ppss)))
          (while (and (< (point) end)
                      (re-search-forward rainbow-delimiters--delim-regex end t))
            (let ((delim-pos (match-beginning 0)))
              (setq ppss (save-excursion
                           (parse-partial-sexp last-ppss-pos delim-pos nil nil ppss)))
              (setq last-ppss-pos delim-pos)
              (unless (rainbow-delimiters--char-ineligible-p delim-pos ppss)
                (let* ((delim (char-after delim-pos))
                       (opening-delim-info
                        (assq delim rainbow-delimiters--opening-delim-info)))
                  (if opening-delim-info
                      (progn
                        (setq depth (1+ depth))
                        (rainbow-delimiters--apply-color (cdr opening-delim-info)
                                                         depth
                                                         delim-pos
                                                         t))
                    ;; Not an opening delimiter, so it's a closing delimiter.
                    (let ((closing-delim-info
                           (assq delim rainbow-delimiters--closing-delim-info))
                          (matching-opening-delim (char-after (nth 1 ppss))))
                      (rainbow-delimiters--apply-color (nthcdr 2 closing-delim-info)
                                                       depth
                                                       delim-pos
                                                       (eq (nth 1 closing-delim-info)
                                                           matching-opening-delim))
                      (setq depth (if (<= depth 0)
                                      0 ; unmatched delim
                                    (1- depth)))))))))))))
  ;; We already fontified the delimiters, tell font-lock there's nothing more
  ;; to do.
  nil)

;;; Minor mode:

;; NB: no face defined here because we apply the faces ourselves instead of
;; leaving that to font-lock.
(defconst rainbow-delimiters--font-lock-keywords
  '(rainbow-delimiters--propertize))

(defun rainbow-delimiters--mode-turn-on ()
  "Set up `rainbow-delimiters-mode'."
  ;; Flush the ppss cache now in case there's something left in there.
  (setq rainbow-delimiters--parse-partial-sexp-cache nil)
  (add-hook 'before-change-functions #'rainbow-delimiters--syntax-ppss-flush-cache t t)
  (add-hook 'change-major-mode-hook #'rainbow-delimiters--mode-turn-off nil t)
  (font-lock-add-keywords nil rainbow-delimiters--font-lock-keywords 'append)
  (set (make-local-variable 'jit-lock-contextually) t)
  ;; Create necessary syntax tables inheriting from current major-mode.
  (set (make-local-variable 'rainbow-delimiters--syntax-table)
       (rainbow-delimiters--make-syntax-table (syntax-table))))

(defun rainbow-delimiters--mode-turn-off ()
  "Tear down `rainbow-delimiters-mode'."
  (kill-local-variable 'rainbow-delimiters--syntax-table)
  (font-lock-remove-keywords nil rainbow-delimiters--font-lock-keywords)
  (remove-hook 'change-major-mode-hook #'rainbow-delimiters--mode-turn-off t)
  (remove-hook 'before-change-functions #'rainbow-delimiters--syntax-ppss-flush-cache t))

;;;###autoload
(define-minor-mode rainbow-delimiters-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  nil "" nil ; No modeline lighter - it's already obvious when the mode is on.
  (if rainbow-delimiters-mode
      (rainbow-delimiters--mode-turn-on)
    (rainbow-delimiters--mode-turn-off))
  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

;;;###autoload
(defun rainbow-delimiters-mode-enable ()
  "Enable `rainbow-delimiters-mode'."
  (rainbow-delimiters-mode 1))

;;;###autoload
(defun rainbow-delimiters-mode-disable ()
  "Disable `rainbow-delimiters-mode'."
  (rainbow-delimiters-mode 0))

(provide 'rainbow-delimiters)
;;; rainbow-delimiters.el ends here
