;;; Scheme
;;; This file is part of the Emacs Dev Kit

;; Scheme - note that GDS is guile specific
;; and that it's bundled with Guile in recent versions
(require 'gds)

(setq scheme-program-name "guile")
(add-hook 'scheme-mode-hook 'lisp-coding-hook)

(provide 'scheme-config)
