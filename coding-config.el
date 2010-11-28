;; common configuration for most programming modes

;; this will get applied to all modes extending cc-mode
;; like java-mode, php-mode, etc
(defun c-coding-hook ()
  (setq c-basic-offset 4)
  (coding-hook))

(add-hook 'c-mode-common-hook 'c-coding-hook)

;; ECB configuration
(require 'ecb)

(provide 'coding-config)
