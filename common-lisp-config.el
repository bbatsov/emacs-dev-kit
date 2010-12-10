;; location of Common Lisp executable *not a folder*
(setq inferior-lisp-program "/usr/bin/clisp")
(setq slime-lisp-implementations
      ’((cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl") :coding-system utf-8-unix)))
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(add-hook 'lisp-mode-hook             'lisp-coding-hook)

(add-hook 'slime-repl-mode-hook       'interactive-lisp-coding-hook)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(provide 'common-lisp-config)
