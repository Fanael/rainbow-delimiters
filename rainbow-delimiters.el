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
    ))

(defun rainbow-delimiters--escaped-char-predicate-emacs-lisp (loc)
  "Non-nil iff the character at LOC is escaped as per Emacs Lisp rules."
  (and (eq (char-before loc) ?\?)      ; e.g. ?) - deprecated, but people use it
       (not (and (eq (char-before (1- loc)) ?\\) ; special case: ignore ?\?
                 (eq (char-before (- loc 2)) ?\?)))
       ;; Treat the ? as a quote character only when it starts a symbol, so
       ;; we're not confused by (foo?), which is a valid function call.
       (let ((inhibit-changing-match-data t))
         (save-excursion
           (goto-char (1- loc))
           (looking-at "\\_<")))))

(defun rainbow-delimiters--char-ineligible-p (loc ppss delim-syntax-code)
  "Return non-nil if char at LOC should not be highlighted.
PPSS is the `parse-partial-sexp' state at LOC.
DELIM-SYNTAX-CODE is the `car' of a raw syntax descriptor at LOC.
Characters are considered ineligible if they're in a string, in a comment or
follows an escaping character as per the syntax table."
  (or
   (nth 3 ppss)                ; inside string?
   (nth 4 ppss)                ; inside comment?
   (nth 5 ppss)                ; escaped?
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
     nil))))

(defun rainbow-delimiters--char-escaped-p (loc)
  "Return non-nil if the character at LOC is escaped.
`rainbow-delimiters-escaped-char-predicate' is used to determine if the
character is escaped."
  (when rainbow-delimiters-escaped-char-predicate
    (funcall rainbow-delimiters-escaped-char-predicate loc)))

(defvar rainbow-delimiters--syntax-ppss-cache nil)
(make-variable-buffer-local 'rainbow-delimiters--syntax-ppss-cache)
(defvar rainbow-delimiters--syntax-ppss-last nil)
(make-variable-buffer-local 'rainbow-delimiters--syntax-ppss-last)

(defvar rainbow-delimiters--category-propertize-done -1
  "The position up to which the disabled delimiter category is set up.")
(make-variable-buffer-local 'rainbow-delimiters--category-propertize-done)

(defvar rainbow-delimiters--original-unfontify-region-function nil)
(make-variable-buffer-local 'rainbow-delimiters--original-unfontify-region-function)

(defun rainbow-delimiters--unfontify-region (start end)
  "Remove the `rainbow-delimiters-disabled-delim' category in START..END."
  (let ((pos (next-single-property-change start 'category nil end)))
    (while (< pos end)
      (let ((range-end (next-single-property-change pos 'category nil end)))
        (when (eq (get-text-property pos 'category) 'rainbow-delimiters-disabled-delim)
          (remove-list-of-text-properties pos range-end '(category)))
        (setq pos range-end))))
  (setq rainbow-delimiters--category-propertize-done
        (min (1- start) rainbow-delimiters--category-propertize-done))
  (funcall rainbow-delimiters--original-unfontify-region-function start end))

(defconst rainbow-delimiters--delim-regex "\\s(\\|\\s)"
  "Regex matching all opening and closing delimiters the mode highlights.")

(defun rainbow-delimiters--for-all-delimiters (end func)
  "For all delimiters between point and END, call FUNC.

FUNC is called with four arguments (DELIM-POS DELIM-SYNTAX PPSS DEPTH), where:
 - DELIM-POS is the position of the delimiter.
 - DELIM-SYNTAX is the raw syntax descriptor of the character at DELIM-POS.
 - PPSS is the `parse-partial-sexp' state at DELIM-POS.
 - DEPTH is the value returned by the previous call to FUNC, or (max 0 (nth 0
   ppss)) if it's the first call.

Ineligible delimiters are skipped."
  (declare (indent defun))
  (let* ((ppss (syntax-ppss))
         (last-ppss-pos (point))
         (depth (max 0 (nth 0 ppss))))
    (while (re-search-forward rainbow-delimiters--delim-regex end t)
      (let* ((delim-pos (match-beginning 0))
             (delim-syntax (syntax-after delim-pos)))
        (setq ppss (save-excursion
                     (parse-partial-sexp last-ppss-pos delim-pos nil nil ppss)))
        (setq last-ppss-pos delim-pos)
        (unless (rainbow-delimiters--char-ineligible-p delim-pos ppss (car delim-syntax))
          (setq depth (funcall func delim-pos delim-syntax ppss depth)))))))

(defun rainbow-delimiters--category-propertize (end)
  "Ensure that `rainbow-delimiters-disabled-delim' category is set up to END."
  (when (< rainbow-delimiters--category-propertize-done end)
    (let ((inhibit-point-motion-hooks t))
      (save-excursion
        (goto-char (min (point) (1+ rainbow-delimiters--category-propertize-done)))
        (rainbow-delimiters--for-all-delimiters end
          (lambda (delim-pos _delim-syntax _ppss _depth)
            (when (or
                   (rainbow-delimiters--char-escaped-p delim-pos)
                   (memq (char-after delim-pos) rainbow-delimiters-delimiter-blacklist))
              (put-text-property delim-pos
                                 (1+ delim-pos)
                                 'category
                                 'rainbow-delimiters-disabled-delim))))))
    (setq rainbow-delimiters--category-propertize-done end)))

(defun rainbow-delimiters--face-propertize* (end)
  "Apply faces to the eligible delimiters between point and END.

Should always be called with the `syntax-ppss' using our internal version of the
cache and with the `rainbow-delimiters-disabled-delim' category armed."
  (let ((inhibit-point-motion-hooks t)
        (parse-sexp-lookup-properties t))
    (rainbow-delimiters--for-all-delimiters end
      (lambda (delim-pos delim-syntax ppss depth)
        (cond
         ((= 4 (logand #xFFFF (car delim-syntax)))
          (let ((new-depth (1+ depth)))
            (rainbow-delimiters--apply-color delim-pos new-depth t)
            new-depth))
         (t
          (let* ((opening-delim (char-after (nth 1 ppss)))
                 (matching-p (eq opening-delim (cdr delim-syntax))))
            (rainbow-delimiters--apply-color delim-pos depth matching-p)
            ;; Don't let depth go negative, even if there's an unmatched
            ;; delimiter.
            (max 0 (1- depth))))))))
  ;; We already fontified the delimiters, tell font-lock there's nothing more
  ;; to do.
  nil)

(defun rainbow-delimiters--face-propertize (end)
  "Apply faces to the eligible delimiters between point and END.

Should always be called with `syntax-begin-function' and
`font-lock-beginning-of-syntax-function' bound to nil."
  ;; Switch the `syntax-ppss' cache and arm the category.
  (let ((oldcache syntax-ppss-cache)
        (oldlast syntax-ppss-last))
    (setq syntax-ppss-cache rainbow-delimiters--syntax-ppss-cache)
    (setq syntax-ppss-last rainbow-delimiters--syntax-ppss-last)
    (setplist 'rainbow-delimiters-disabled-delim '(syntax-table (1)))
    (unwind-protect
        (rainbow-delimiters--face-propertize* end)
      (setplist 'rainbow-delimiters-disabled-delim '())
      (setq rainbow-delimiters--syntax-ppss-cache syntax-ppss-cache)
      (setq rainbow-delimiters--syntax-ppss-last syntax-ppss-last)
      (setq syntax-ppss-cache oldcache)
      (setq syntax-ppss-last oldlast))))

;; Main function called by font-lock.
(defun rainbow-delimiters--propertize (end)
  "Highlight delimiters in region between point and END.

Used by font-lock for dynamic highlighting."
  (setq rainbow-delimiters-escaped-char-predicate
        (cdr (assoc major-mode rainbow-delimiters-escaped-char-predicate-list)))
  (rainbow-delimiters--category-propertize end)
  (with-no-warnings
    (let ((syntax-begin-function nil)
          (font-lock-beginning-of-syntax-function nil))
      (rainbow-delimiters--face-propertize end))))

;; NB: no face defined here because we apply the faces ourselves instead of
;; leaving that to font-lock.
(defconst rainbow-delimiters--font-lock-keywords
  '(rainbow-delimiters--propertize))

;;;###autoload
(define-minor-mode rainbow-delimiters-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  nil "" nil ; No modeline lighter - it's already obvious when the mode is on.
  (font-lock-remove-keywords nil rainbow-delimiters--font-lock-keywords)
  (kill-local-variable 'rainbow-delimiters--syntax-ppss-cache)
  (kill-local-variable 'rainbow-delimiters--syntax-ppss-last)
  (kill-local-variable 'rainbow-delimiters--syntax-propertize-done)
  (when rainbow-delimiters--original-unfontify-region-function
    (set (make-local-variable 'font-lock-unfontify-region-function)
         rainbow-delimiters--original-unfontify-region-function))
  (kill-local-variable 'rainbow-delimiters--original-unfontify-region-function)
  (when rainbow-delimiters-mode
    (font-lock-add-keywords nil rainbow-delimiters--font-lock-keywords 'append)
    (setq rainbow-delimiters--original-unfontify-region-function
          font-lock-unfontify-region-function)
    (set (make-local-variable 'font-lock-unfontify-region-function)
         #'rainbow-delimiters--unfontify-region))
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
