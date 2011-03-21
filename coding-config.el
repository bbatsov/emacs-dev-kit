;; common configuration for most programming modes

;; this will get applied to all modes extending cc-mode
;; like java-mode, php-mode, etc
(defun c-coding-hook ()
  (setq c-basic-offset 4)
  (coding-hook))

(add-hook 'c-mode-common-hook 'c-coding-hook)

;; ECB configuration
(require 'ecb)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(require 'rainbow-delimiters)

;; a great lisp coding hook
(defun lisp-coding-hook ()
  (coding-hook)
  (setq autopair-dont-activate t)
  (paredit-mode +1)
  (rainbow-delimiters-mode))

;; interactive modes don't need whitespace checks
(defun interactive-lisp-coding-hook ()
  (setq autopair-dont-activate t)
  (paredit-mode +1)
  (rainbow-delimiters-mode)
  (turn-off-whitespace))

(provide 'coding-config)
