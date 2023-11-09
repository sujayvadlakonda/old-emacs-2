;; -*- lexical-binding: t; -*-

;; Bind M-. to `elisp-slime-nav-find-elisp-thing-at-point'
;; Bind C-c C-d C-d to `elisp-slime-nav-describe-elisp-thing-at-point'
(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook help-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; Bind C-c C-l to `eval-buffer' and `eval-region'
(defun sanityinc/load-this-file ()
  "Load the region or current file or buffer.
The current directory is temporarily added to `load-path'.  When
there is no current file, eval the current buffer."
  (interactive)
  (let ((load-path (cons default-directory load-path))
        (file (buffer-file-name)))
    ;; Call `eval-region' when the region is active
    (if (region-active-p)
        (eval-region)
      ;; Else `eval-buffer'
      (if file
          (progn
            (save-some-buffers nil (apply-partially 'derived-mode-p 'emacs-lisp-mode))
            (load-file (buffer-file-name))
            (message "Loaded %s" file))
        (eval-buffer)
        (message "Evaluated %s" (current-buffer))))))

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'sanityinc/load-this-file))
