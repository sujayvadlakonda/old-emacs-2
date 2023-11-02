;; -*- lexical-binding: t; -*-

;; Turn off paredit, after paredit is turned on
(add-hook 'emacs-lisp-mode-hook 'disable-paredit-mode 100)

;; jump to def, and describe at point
(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook help-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
