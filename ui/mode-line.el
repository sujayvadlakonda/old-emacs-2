;; -*- lexical-binding: t; -*-

(require-package 'doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)

;; We display project info in the modeline ourselves
(setq projectile-dynamic-mode-line nil)
;; Set these early so they don't trigger variable watchers
(setq doom-modeline-bar-width 3
      doom-modeline-github nil
      doom-modeline-mu4e nil
      doom-modeline-persp-name nil
      doom-modeline-minor-modes nil
      doom-modeline-major-mode-icon nil
      doom-modeline-buffer-file-name-style 'relative-from-project
      ;; Only show file encoding if it's non-UTF-8 and different line endings
      ;; than the current OSes preference
      doom-modeline-buffer-encoding 'nondefault
      doom-modeline-default-eol-type
      (cond ((eq system-type 'darwin) 2)
            ((memq system-type '(cygwin windows-nt ms-dos)) 1)
            (0)))
