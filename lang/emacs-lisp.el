;; -*- lexical-binding: t; -*-

;; jump to def, and describe at point
(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook help-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
