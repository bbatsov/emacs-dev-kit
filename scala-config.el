;;; Scala
;;; This file is part of the Emacs Dev Kit

(require 'scala-mode-auto)

(add-hook 'scala-mode-hook (lambda () (subword-mode)))

(provide 'scala-config)
