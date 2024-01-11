;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

(maybe-require-package 'list-unicode-display)


;;; Some basic preferences

(setq-default
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines t
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)



;; Huge files

(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

(require-package 'vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;; Do not blink cursor
(setq visible-cursor nil)
(blink-cursor-mode 0)


;; Highlight the line to immediately find cursor
(add-hook 'after-init-hook #'global-hl-line-mode)


;;; A simple visible bell which works in all terminal types
(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)



;;; Newline behaviour (see also electric-indent-mode, enabled above)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))



(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?┊)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))



(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))


;;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))


;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

(when (fboundp 'repeat-mode)
  (add-hook 'after-init-hook 'repeat-mode))


;;; Handy key bindings

(with-eval-after-load 'help
  (define-key help-map "A" 'describe-face))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)



;;; Page break lines

(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode))



;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

(require-package 'move-dup)
(global-set-key [M-S-up] 'move-dup-move-lines-up)
(global-set-key [M-S-down] 'move-dup-move-lines-down)

(global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
(global-set-key (kbd "C-c u") 'move-dup-duplicate-up)


;;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up



;;; Cut/copy the current line if no region is active
(require-package 'whole-line-or-region)
(add-hook 'after-init-hook 'whole-line-or-region-global-mode)



(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)



;; M-^ is inconvenient, so also bind M-j
(global-set-key (kbd "M-j") 'join-line)


;; Random line sorting
(defun sanityinc/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))


(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)


(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(setq-default which-key-idle-delay 1.5)


(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)

;; Prefer C-h for `delete-backward-char' to `help-map'
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))

;; Use `back-to-indentation' instead of `move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'back-to-indentation)

(require-package 'multistate)
(multistate-global-mode)

(cl-defun define-state (name &key map (cursor t))
  "Define state while ignoring if state already exists."
  (ignore-errors
    (multistate-define-state name :parent map :cursor cursor)))

(define-state 'motion :map 'multistate-suppress-map)
(define-kbd multistate-motion-state-map "j" 'backward-char)
(define-kbd multistate-motion-state-map "k" 'next-line)
(define-kbd multistate-motion-state-map "l" 'previous-line)
(define-kbd multistate-motion-state-map ";" 'forward-char)

(define-kbd multistate-motion-state-map "z" 'execute-extended-command)

(defvar sujay/leader-map (make-sparse-keymap))
(define-kbd multistate-motion-state-map "SPC" sujay/leader-map)
(define-kbd sujay/leader-map "b" 'consult-buffer)
(define-kbd sujay/leader-map "f" 'find-file)
(define-kbd sujay/leader-map "s" 'save-buffer)
(define-kbd sujay/leader-map "0" 'delete-window)
(define-kbd sujay/leader-map "1" 'sanityinc/toggle-delete-other-windows)
(define-kbd sujay/leader-map "2" (split-window-func-with-other-buffer 'split-window-vertically))
(define-kbd sujay/leader-map "3" (split-window-func-with-other-buffer 'split-window-horizontally))
(define-kbd sujay/leader-map "o" 'switch-window)
(define-kbd sujay/leader-map "a" 'sujay/org-agenda)
(define-kbd sujay/leader-map "c" 'sujay/org-capture)
(define-kbd sujay/leader-map "g" 'magit)
(define-kbd sujay/leader-map "B" 'ibuffer)
(define-kbd sujay/leader-map "k" 'kill-this-buffer)
(define-kbd sujay/leader-map "m" 'increment-medicine)
(define-kbd sujay/leader-map "w" 'push-window-configuration)
(define-kbd sujay/leader-map "W" 'pop-window-configuration)

(define-state 'insert :cursor 'bar)
(define-kbd multistate-insert-state-map "ESC" 'multistate-normal-state)

(define-state 'normal :map 'multistate-motion-state-map)
(define-kbd multistate-normal-state-map "i" 'multistate-insert-state)

(require-package 'whole-line-or-region)
(define-kbd multistate-normal-state-map "d" 'whole-line-or-region-kill-region)
(define-kbd multistate-normal-state-map "y" 'whole-line-or-region-kill-ring-save)

(define-kbd multistate-normal-state-map "p" 'yank)
(define-kbd multistate-normal-state-map "u" 'undo)

(require-package 'multiple-cursors)
(define-kbd multistate-normal-state-map "n" 'mc/mark-next-like-this)
(define-kbd multistate-normal-state-map "m" 'mc/mark-previous-like-this)
(define-kbd multistate-normal-state-map "*" 'mc/mark-all-like-this)

(require-package 'meow)
(require 'meow)
(define-kbd multistate-normal-state-map "w" 'meow-next-word)
(define-kbd multistate-normal-state-map "b" 'meow-back-word)

(define-kbd multistate-normal-state-map "o" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (end-of-line)
                                              (newline-and-indent)))

(define-kbd multistate-normal-state-map "O" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (beginning-of-line)
                                              (newline-and-indent)
                                              (previous-line)
                                              (indent-for-tab-command)))

(define-kbd multistate-normal-state-map "x" 'delete-char)

(add-hook 'prog-mode-hook 'multistate-normal-state)
(add-hook 'text-mode-hook 'multistate-normal-state)
;; (define-kbd modalka-mode-map "i" (lambda ()
;;                                    (interactive)
;;                                    (deactivate-mark)
;;                                    (modalka-mode -1)))

;; (define-kbd modalka-mode-map "a" (lambda ()
;;                                    (interactive)
;;                                    (modalka-mode -1)
;;                                    (forward-char)))

;; (define-kbd modalka-mode-map "v" 'set-mark-command)
(define-kbd multistate-normal-state-map "V" (lambda ()
                                              (interactive)
                                              (beginning-of-line)
                                              (set-mark-command nil)
                                              (end-of-line)
                                              (forward-char)))

(define-kbd multistate-normal-state-map "A" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (end-of-line)))
(define-kbd multistate-normal-state-map "I" (lambda ()
                                              (interactive)
                                              (multistate-insert-state)
                                              (back-to-indentation)))

;; (define-kbd modalka-mode-map "I" (lambda ()
;;                                    (interactive)
;;                                    (modalka-mode -1)
;;                                    (back-to-indentation)))

(define-kbd multistate-normal-state-map "c" (lambda ()
                                              (interactive)
                                              (whole-line-or-region-kill-region 1)
                                              (multistate-insert-state)))
(define-kbd multistate-normal-state-map "C" (lambda ()
                                              (interactive)
                                              (kill-line)
                                              (multistate-insert-state)))

;; (define-kbd modalka-mode-map "gg" 'beginning-of-buffer)
;; (define-kbd modalka-mode-map "G" 'end-of-buffer)

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
