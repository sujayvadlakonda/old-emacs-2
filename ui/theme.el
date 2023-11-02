;; -*- lexical-binding: t; -*-

(require-package 'ef-themes)

(defcustom theme 'ef-elea-dark
  "Theme to load on startup")

(defun load-theme! ()
  (when (and theme
             (not (custom-theme-enabled-p theme)))
    (load-theme theme t)))

(add-hook 'after-init-hook 'load-theme! -90)


(advice-add 'load-theme! :after (lambda (&rest _args)
                                     (set-face-bold 'bold nil)
                                     (set-face-italic 'italic nil)))


(when (maybe-require-package 'dimmer)
  ;; Do not dim entire frame when out of focus
  (setq-default dimmer-watch-frame-focus-events nil)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))
