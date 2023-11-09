;; -*- lexical-binding: t; -*-

(defconst OS-MAC (eq system-type 'darwin))
(defconst OS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defun load! (relative-file-path)
  "Load file at RELATIVE-FILE-PATH to `user-emacs-directory'"
  (load (concat user-emacs-directory relative-file-path) nil t))

(defun global-set-kbd (key command)
  (global-set-key (kbd key) command))
