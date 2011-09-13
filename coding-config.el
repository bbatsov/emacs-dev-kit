;;; common configuration for most programming modes
;;; This file is part of the Emacs Dev Kit

;; this will get applied to all modes extending cc-mode
;; like java-mode, php-mode, etc
(defun c-coding-hook ()
  (setq c-basic-offset 4)
  (coding-hook))

(add-hook 'c-mode-common-hook 'c-coding-hook)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; a great lisp coding hook
(defun lisp-coding-hook ()
  (coding-hook)
  (setq autopair-dont-activate t)
  (paredit-mode +1)
)

;; interactive modes don't need whitespace checks
(defun interactive-lisp-coding-hook ()
  (setq autopair-dont-activate t)
  (paredit-mode +1)
  (turn-off-whitespace))

(provide 'coding-config)
