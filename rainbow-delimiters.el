;;; rainbow-delimiters.el --- Highlight nested parens, brackets, braces a different color at each depth. -*- lexical-binding: t -*-

;; Copyright (C)
;;   2010-2013 Jeremy Rayman
;;   2013-2014 Fanael Linithien
;; Author: Jeremy Rayman <opensource@jeremyrayman.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; Maintainer: Fanael Linithien <fanael4@gmail.com>
;; Created: 2010-09-02
;; Version: 1.3.13
;; Keywords: faces, convenience, lisp, tools
;; Homepage: https://github.com/Fanael/rainbow-delimiters

;; Note: despite `lexical-binding', there's no Emacs 24 dependency.
;; This is merely an optimization for Emacs 24+, the code is supposed to work
;; with *both* dynamic and lexical binding.

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
;; Rainbow-delimiters is a "rainbow parentheses"-like mode which highlights
;; parentheses, brackets, and braces according to their depth. Each
;; successive level is highlighted in a different color. This makes it easy
;; to spot matching delimiters, orient yourself in the code, and tell which
;; statements are at a given level.
;;
;; Great care has been taken to make this mode fast. You shouldn't see
;; any discernible change in scrolling or editing speed while using it,
;; even in delimiter-rich languages like Clojure, Lisp, and Scheme.

;;; Installation:

;; The recommended way is to use MELPA (http://melpa.org/) or MELPA Stable
;; (http://stable.melpa.org/). If either is in your `package-archives', do
;;   M-x package-install RET rainbow-delimiters RET
;; Otherwise, open `rainbow-delimiters.el' in Emacs and use
;;   M-x package-install-from-buffer
;; Any other methods of installation are unsupported.
;;
;; To toggle the mode in the current buffer:
;;   M-x rainbow-delimiters-mode
;; To start the mode automatically in `foo-mode', add the following to your init
;; file:
;;   (add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
;; To start the mode automatically in most programming modes (Emacs 24 and
;; above):
;;   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Customization:

;; To customize various options, including the color theme:
;;   M-x customize-group rainbow-delimiters
;;
;; You can specify custom colors by customizing following faces:
;; - Faces take the form `rainbow-delimiters-depth-N-face', with N being the
;;   depth. Depth begins at 1, the outermost color. Faces exist for depths 1-9.
;; - The unmatched delimiter face: `rainbow-delimiters-unmatched-face'.
;; - The mismatched delimiter face: `rainbow-delimiters-mismatched-face'.

;;; Code:

;;; Customize interface:

(defgroup rainbow-delimiters nil
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :prefix "rainbow-delimiters-"
  :link '(url-link :tag "Website for rainbow-delimiters"
                   "https://github.com/Fanael/rainbow-delimiters")
  :group 'applications)

(defgroup rainbow-delimiters-faces nil
  "Faces for successively nested pairs of delimiters.

When depth exceeds innermost defined face, colors cycle back through."
  :group 'rainbow-delimiters
  :group 'faces
  :link '(custom-group-link "rainbow-delimiters")
  :prefix "rainbow-delimiters-")

(defcustom rainbow-delimiters-delimiter-blacklist '()
  "Disable highlighting of selected delimiters.

Delimiters in this list are not highlighted."
  :tag "Delimiter Blacklist"
  :type '(repeat character)
  :group 'rainbow-delimiters)

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

(eval-when-compile
  (defmacro rainbow-delimiters--define-depth-faces ()
    (let ((faces '())
          (light-colors ["#707183" "#7388d6" "#909183" "#709870" "#907373"
                         "#6276ba" "#858580" "#80a880" "#887070"])
          (dark-colors ["grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
                        "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad"]))
      (dotimes (i 9)
        (push `(defface ,(intern (format "rainbow-delimiters-depth-%d-face" (1+ i)))
                 '((((class color) (background light)) :foreground ,(aref light-colors i))
                   (((class color) (background dark)) :foreground ,(aref dark-colors i)))
                 ,(format "Nested delimiter face, depth %d." (1+ i))
                 :group 'rainbow-delimiters-faces)
              faces))
      `(progn ,@faces))))
(rainbow-delimiters--define-depth-faces)

(defcustom rainbow-delimiters-max-face-count 9
  "Number of faces defined for highlighting delimiter levels.

Determines depth at which to cycle through faces again.

It's safe to change this variable provided that for all integers from 1 to the
new value inclusive, a face `rainbow-delimiters-depth-N-face' is defined."
  :type 'integer
  :group 'rainbow-delimiters)

(defcustom rainbow-delimiters-outermost-only-face-count 0
  "Number of faces to be used only for N outermost delimiter levels.

This should be smaller than `rainbow-delimiters-max-face-count'."
  :type 'integer
  :group 'rainbow-delimiters)


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

(defun rainbow-delimiters--apply-color (loc depth match)
  "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.
MATCH is nil iff it's a mismatched closing delimiter.

The delimiter is not highlighted if it's a blacklisted delimiter."
  (unless (memq (char-after loc) rainbow-delimiters-delimiter-blacklist)
    (let ((delim-face (cond
                       ((<= depth 0)
                        'rainbow-delimiters-unmatched-face)
                       ((not match)
                        'rainbow-delimiters-mismatched-face)
                       (t
                        (rainbow-delimiters--depth-face depth)))))
      (font-lock-prepend-text-property loc (1+ loc) 'face delim-face))))

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

(defun rainbow-delimiters--char-ineligible-p (loc ppss delim-syntax-code)
  "Return t if char at LOC should not be highlighted.
PPSS is the `parse-partial-sexp' state at LOC.
DELIM-SYNTAX-CODE is the `car' of a raw syntax descriptor at LOC.

Returns t if char at loc meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
  (or
   (nth 3 ppss)                ; inside string?
   (nth 4 ppss)                ; inside comment?
   ;; Note: no need to consider single-char openers, they're already handled
   ;; by looking at ppss.
   (cond
    ;; Two character opener, LOC at the first character?
    ((/= 0 (logand #x10000 delim-syntax-code))
     (/= 0 (logand #x20000 (or (car (syntax-after (1+ loc))) 0))))
    ;; Two character opener, LOC at the second character?
    ((/= 0 (logand #x20000 delim-syntax-code))
     (/= 0 (logand #x10000 (or (car (syntax-after (1- loc))) 0))))
    (t
     nil))
   (when rainbow-delimiters-escaped-char-predicate
     (funcall rainbow-delimiters-escaped-char-predicate loc))))

(defconst rainbow-delimiters--delim-regex "\\s(\\|\\s)"
  "Regex matching all opening and closing delimiters the mode highlights.")

;; Main function called by font-lock.
(defun rainbow-delimiters--propertize (end)
  "Highlight delimiters in region between point and END.

Used by font-lock for dynamic highlighting."
  (setq rainbow-delimiters-escaped-char-predicate
        (cdr (assoc major-mode rainbow-delimiters-escaped-char-predicate-list)))
  (let* ((inhibit-point-motion-hooks t)
         ;; Point can be anywhere in buffer; determine the nesting depth at point.
         (last-ppss-pos (point))
         (ppss (syntax-ppss))
         ;; Ignore negative depths created by unmatched closing delimiters.
         (depth (max 0 (nth 0 ppss))))
    (while (re-search-forward rainbow-delimiters--delim-regex end t)
      (let* ((delim-pos (match-beginning 0))
             (delim-syntax (syntax-after delim-pos)))
        (setq ppss (save-excursion
                     (parse-partial-sexp last-ppss-pos delim-pos nil nil ppss)))
        (setq last-ppss-pos delim-pos)
        (unless (rainbow-delimiters--char-ineligible-p delim-pos ppss (car delim-syntax))
          (if (= 4 (logand #xFFFF (car delim-syntax)))
              (progn
                (setq depth (1+ depth))
                (rainbow-delimiters--apply-color delim-pos
                                                 depth
                                                 t))
            ;; Not an opening delimiter, so it's a closing delimiter.
            (let ((matching-opening-delim (char-after (nth 1 ppss))))
              (rainbow-delimiters--apply-color delim-pos
                                               depth
                                               (eq (cdr delim-syntax)
                                                   matching-opening-delim))
              ;; Don't let `depth' go negative, even if there's an unmatched
              ;; delimiter.
              (setq depth (max 0 (1- depth)))))))))
  ;; We already fontified the delimiters, tell font-lock there's nothing more
  ;; to do.
  nil)

;; NB: no face defined here because we apply the faces ourselves instead of
;; leaving that to font-lock.
(defconst rainbow-delimiters--font-lock-keywords
  '(rainbow-delimiters--propertize))

;;;###autoload
(define-minor-mode rainbow-delimiters-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  nil "" nil ; No modeline lighter - it's already obvious when the mode is on.
  (font-lock-remove-keywords nil rainbow-delimiters--font-lock-keywords)
  (when rainbow-delimiters-mode
    (font-lock-add-keywords nil rainbow-delimiters--font-lock-keywords 'append)
    (set (make-local-variable 'jit-lock-contextually) t)
    ;; `syntax-begin-function' may break the assumption we rely on that
    ;; `syntax-ppss' is exactly equivalent to `parse-partial-sexp' from
    ;; `point-min'. Just don't use it, the performance hit should be negligible.
    (set (make-local-variable 'syntax-begin-function) nil)
    ;; Obsolete equivalent of `syntax-begin-function'.
    (when (boundp 'font-lock-beginning-of-syntax-function)
      (with-no-warnings
        (set (make-local-variable 'font-lock-beginning-of-syntax-function) nil)))
    ;; We modified `syntax-begin-function', so flush the cache to avoid getting
    ;; cached values that used the old value.
    (syntax-ppss-flush-cache 0))
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
