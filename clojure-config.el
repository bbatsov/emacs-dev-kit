(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'lisp-coding-hook)

;; slime-autodoc-mode is causing the Clojure REPL's to hang
;; so we need to disable it
(add-hook 'slime-connected-hook
          (lambda ()
            (if (string= (slime-lisp-implementation-type) "Clojure")
                (setq slime-use-autodoc-mode nil)
              (setq slime-use-autodoc-mode t))
            ))

(add-hook 'slime-mode-hook
          (lambda ()
            (if (eq major-mode 'clojure-mode)
                (slime-autodoc-mode 0)
              (slime-autodoc-mode 1))))

;; ugly solution, because the first hook doesn't work
;(setq slime-use-autodoc-mode nil)

(provide 'clojure-config)
