# `easy-escape`

![Easy-escape: before and after](img/easy-escape.png)

`easy-escape-minor-mode` uses syntax highlighting and composition to make ELisp regular expressions more readable.  More precisely, it hides double backslashes preceding regexp specials (`()|`), composes other double backslashes into single ones, and applies a special face to each. The underlying buffer text is not modified.

The default is to use a single \ character instead of two, and to hide backslashes preceding parentheses or `|`.  The escape character and its color can be customized using `easy-escape-face` and `easy-escape-character` (which see), and backslashes before `()|` can be shown by disabling `easy-escape-hide-escapes-before-delimiters`.

## Setup

### MELPA (preferred)

1. Setup [MELPA](http://melpa.org/#/getting-started) if you haven't yet

    In your `.emacs`, add these three lines:

    ```elisp
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    (package-initialize)
    ```

2. Install the package: `M-x package-install RET easy-escape RET`

3. Enable `easy-escape-minor-mode` in Lisp buffers:

    ```elisp
    ;; Replace 'lisp-mode-hook with 'prog-mode-hook to enable everywhere
    (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)
    ```

### Alternative setup

1. Download [this file](https://raw.githubusercontent.com/cpitclaudel/easy-escape/master/easy-escape.el)

2. Add this to your .emacs:

    ```elisp
    (load-file "PATH-TO-THE-FILE-YOU-JUST-DOWNLOADED")
    (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
    ```

## Fine-tuning

* If you find the distinction between the fontified double-slash and the
single slash too subtle, try the following:

    * Adjust the foreground of `easy-escape-face`:

        ```elisp
        (set-face-attribute 'easy-escape-face nil :foreground "red")
        ```

    * Set `easy-escape-character` to a different character:

        ```elisp
        (setq easy-escape-character ?╲)
        ```
