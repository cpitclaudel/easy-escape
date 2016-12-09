;;; ee-screenshot.el --- Prepare a screenshot for easy-escape  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement@clem-w50-mint>
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

;; emacs -Q -l ee-screenshot.el -f '~/main'

;;; Code:

(add-to-list 'load-path "../")
(require 'easy-escape)

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

(defun ~/capture ()
  (force-window-update)
  (redisplay t)
  (let ((png-fname "easy-escape.png"))
    (call-process "import" nil nil nil "-window" (frame-parameter nil 'outer-window-id) png-fname)
    (call-process "mogrify" nil nil nil "-strip" "-matte"
                  "-bordercolor" (face-attribute 'fringe :background)
                  "-border" (format "0x%d" (car fringe-mode))
                  png-fname)
    (call-process "optipng" nil nil nil "-o3" png-fname))
  (kill-emacs))

(defun ~/main ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (load-theme 'tango t)
  (set-fringe-mode (cons 8 8))
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 110)
  (set-face-attribute 'mode-line-buffer-id nil :foreground "#eab700")
  (set-face-attribute 'mode-line nil :foreground "gray60" :background "black")
  (set-face-attribute 'mode-line-inactive nil :foreground "gray60" :background "black")
  (let ((before (get-buffer-create "*before*"))
        (after (get-buffer-create "*after*")))
    (~/setup before -1 "\n ;; Before:")
    (~/setup after 1 " ;; After:")
    (switch-to-buffer before)
    (switch-to-buffer-other-window after))
  ;; (with-current-buffer before
  (setq-default mode-line-format nil)
  (set-frame-size nil 72 4)
  (message nil)
  (run-with-timer 3 nil #'~/capture))

(provide 'ee-screenshot)
;;; ee-screenshot.el ends here
