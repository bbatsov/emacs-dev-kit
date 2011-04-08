;;; Scheme
;;; This file is part of the Emacs Dev Kit

(setq scheme-program-name "guile")
(add-hook 'scheme-mode-hook 'lisp-coding-hook)

(provide 'scheme-config)
