;; Scala programming language support
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook 'coding-hook)

(add-to-list 'load-path (concat ext-dir "ensime_2.8.1-0.4.2/elisp/"))
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook (lambda () (subword-mode)))
;; Make sure that your bin/server.sh has executable permission

(provide 'scala-config)
