;; -*- lexical-binding: t; -*-

;; This file loads before UI and package.el
;; You can *prevent* things from loading,
;; rather than turn them off after-the-fact.

(setq package-enable-at-startup nil)

(load (concat user-emacs-directory "boot.el") nil t)
