;; -*- lexical-binding: t; -*-

(defconst OS-MAC (eq system-type 'darwin))
(defconst OS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
