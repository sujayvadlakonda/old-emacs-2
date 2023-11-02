;; -*- lexical-binding: t; -*-

;; Automatically create missing directories of visited file
(defun create-missing-directories ()
  "Create any missing directories of the visited file."
  (let ((target-directory (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-directory)
      (make-directory target-directory t))))

(add-to-list 'find-file-not-found-functions #'create-missing-directories)

;; Permanently kill scratch buffer
(defun kill-scratch-buffer ()
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*")))

(add-hook 'after-change-major-mode-hook #'kill-scratch-buffer)
