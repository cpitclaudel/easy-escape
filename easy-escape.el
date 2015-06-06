;;; easy-escape.el --- Improve readability of escape characters in regular expressions -*- lexical-binding:t -*-

;; Copyright (C) 2015  Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/company-coq

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

;; `easy-escape-minor-mode' composes double backslashes (escape characters) into
;; single backslashes, and highlights them to improve readability.
;;
;; For example, `easy-escape` displays "\\(?:\\_<\\\\newcommand\\_>\\s-*\\)?"
;; as "\(?:\_<\\newcommand\_>\s-*\)?". The underlying text is not modified.
;;
;; The default it to use a single \ character instead of two, but the character
;; used and its color can be customized using `easy-escape-face' and
;; `easy-escape-character' (which see).
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
  "Character by which \\\\ is replaced when `easy-escape-minor-mode' is active.
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

(defconst easy-escape--keywords
  '((easy-escape--mark-escapes (0 (easy-escape--compose (match-beginning 0)))
                               (0 'easy-escape-face append)))
  "Font-lock keyword list used internally")

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
(define-minor-mode easy-escape-minor-mode
  "Compose escape signs together to make regexps more readable.
When this mode is active, \\\\ in strings is displayed as a
single \\, fontified using `easy-escape-face' and composed into
`easy-escape-character'.

If you find the distinction between the fontified double-slash
and the single slash too subtle, try the following:

* Adjust the foreground of `easy-escape-face'
* Set `easy-escape-character' to a different character."
  :lighter " ez-esc"
  :group 'easy-escape
  (if easy-escape-minor-mode
      (progn (font-lock-add-keywords nil easy-escape--keywords)
             (add-to-list (make-local-variable 'font-lock-extra-managed-props) 'composition))
    (font-lock-remove-keywords nil easy-escape--keywords))
  (font-lock-flush))

;;;###autoload
(add-hook 'lisp-mode-hook 'easy-escape-minor-mode)

(provide 'easy-escape)
;;; easy-escape.el ends here
