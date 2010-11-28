;; Scala programming language support
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook 'coding-hook)

(add-to-list 'load-path (concat ext-dir "ensime/elisp/"))
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; Make sure that your bin/server.sh has executable permission

(provide 'scala-config)
