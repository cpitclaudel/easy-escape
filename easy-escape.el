;;; easy-escape.el --- Improve readability of escape characters in regular expressions -*- lexical-binding:t -*-

;;; Commentary:
;; `easy-escape-minor-mode' composes double backslashes (escape characters) into
;; single backslashes, and highlights them to improve readability.  The default
;; it to use a single \ character instead of two, but the character used and its
;; color can be customized using `easy-escape-face' and `easy-escape-character'
;; (which see).
;;
;; For example, `easy-escape` displays "\\(?:\\_<\\\\newcommand\\_>\\s-*\\)?"
;; as "\(?:\_<\\newcommand\_>\s-*\)?". The underlying text is not modified.
;;
;; Suggested setup:
;;   (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
;; or
;;   (add-hook 'prog-mode-hook 'easy-escape-minor-mode)
;;
;; NOTE: If you find the distinction between the fontified double-slash and the
;; single slash too subtle, try the following:
;;
;; * Adjust the foreground of `easy-escape-face'
;; * Set `easy-escape-character' to a different character.

;;; Code:

(defgroup easy-escape nil
  "Improve readability of escape characters"
  :group 'programming)

(defface easy-escape-face
  '((t :weight bold))
  "Face used to highlight \\\\ in strings"
  :group 'easy-escape)

(defcustom easy-escape-character ?\\
  "Character by which \\\\ is replaced when `easy-escape-mode' is active.
Good candidates include the following:
  \\ REVERSE SOLIDUS (the default, typed as '?\\\\')
  ╲ BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT (typed as '?╲')
  ⟍ MATHEMATICAL FALLING DIAGONAL (typed as '?⟍')
  ⑊ OCR DOUBLE BACKSLASH (typed as '?⑊')
  ⤡ NORTH WEST AND SOUTH EAST ARROW (typed as '?⤡')
  ↘ SOUTH EAST ARROW (typed as '?↘')
  ⇘ SOUTH EAST DOUBLE ARROW (typed as '?⇘')
  ⦥ MATHEMATICAL REVERSED ANGLE WITH UNDERBAR (typed as '?⦥')
  ⦣ MATHEMATICAL REVERSED ANGLE (typed as '?⦣')
  ⧹ BIG REVERSE SOLIDUS (typed as '?⧹')
Most of these characters require non-standard fonts to display properly, however."
  :group 'easy-escape)

(defun easy-escape--in-string-p (pos)
  "Indicate whether POS is inside of a string."
  (let ((face (get-text-property pos 'face)))
    (or (eq 'font-lock-string-face face)
        (and (listp face) (memq 'font-lock-string-face face)))))

(defun easy-escape--mark-escapes (limit)
  "Position point at end of next \\\\, and set match data.
Search ends at LIMIT."
  (catch 'found
    (while (re-search-forward "\\(\\\\\\\\\\)" limit t)
      (when (easy-escape--in-string-p (match-beginning 0))
        (throw 'found t)))))

(defun easy-escape--compose (start)
  "Compose characters from START to (+ 2 START) into `easy-escape-character'."
  (compose-region start (+ 2 start) easy-escape-character))

;;;###autoload
(define-minor-mode easy-escape-mode
  "Compose escape signs together to make regexps more readable.
When this mode is active, \\\\ in strings is displayed as a
single \\, fontified using `easy-escape-face' and composed into
`easy-escape-character'.

If you find the distinction between the fontified double-slash
and the single slash too subtle, try the following:

* Adjust the foreground of `easy-escape-face'
* Set `easy-escape-character' to a different character."
  nil " ez-esc" nil
  (font-lock-add-keywords nil '((easy-escape--mark-escapes (0 (easy-escape--compose (match-beginning 0)))
                                                           (0 'easy-escape-face prepend))))
  (add-to-list 'font-lock-extra-managed-props 'composition))

(provide 'easy-escape)
;;; easy-escape.el ends here
