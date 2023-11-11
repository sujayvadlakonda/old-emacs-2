;; -*- lexical-binding: t; -*-
;; This file loads before UI and package.el
;; You can *prevent* things from loading,
;; rather than turn them off after-the-fact.

;; Prevent package.el from loading
(setq package-enable-at-startup nil)

;; Load the rest of the config
(load (concat user-emacs-directory "boot.el") nil t)
