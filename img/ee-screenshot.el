;;; ee-screenshot.el --- Prepare a screenshot for easy-escape  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.
;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Keywords:

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

;; emacs -Q -l ~/.emacs.d/lisp/screenshot/screenshot.el -l ee-screenshot.el -f '~/main'

;;; Code:

(add-to-list 'load-path "../")
(require 'easy-escape)
(require 'screenshot)

(defvar ~/strs
  '(("^\\([aA]\\)\\(#\\|!\\)\\([ \\t\\n\\v\\r]*\\)[:/]\\(\\\\\\)\\(...\\)$")
    ("\\.\\(gz\\|Z\\|bz\\|bz2\\|xz\\|gpg\\)\\'" "\\((\\||\\|)\\)")))

(defun ~/setup (buf mode-status prelude)
  (with-current-buffer buf
    (emacs-lisp-mode)
    (easy-escape-minor-mode mode-status)
    (insert prelude "\n")
    (dolist (strs ~/strs)
      (insert " " (mapconcat #'prin1-to-string strs "  ") "\n"))
    (setq cursor-type nil)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun ~/main ()
  (interactive)
  (let ((before (get-buffer-create "*before*"))
        (after (get-buffer-create "*after*")))
    (~/setup before -1 "\n ;; Before:")
    (~/setup after 1 " ;; After:")
    (switch-to-buffer before)
    (switch-to-buffer-other-window after))
  ;; (with-current-buffer before
  (setq-default mode-line-format nil)
  (screenshot-ui-setup 72 4)
  (run-with-timer 3 nil #'screenshot-capture "easy-escape.png")
  (run-with-timer 4 nil #'kill-emacs))

(provide 'ee-screenshot)
;;; ee-screenshot.el ends here
