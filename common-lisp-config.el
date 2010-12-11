;;; Common Lisp
;;; This file is part of the Emacs Dev Kit

;; location of default Common Lisp executable *not a folder*
(setq inferior-lisp-program "/usr/bin/clisp -q")
;; a list of alternative Common Lisp implementations that can be
;; used with SLIME
(setq slime-lisp-implementations
      ’((cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
;; delay SLIME loading until you actually need it
(require 'slime-autoloads)
;; slime-fancy is meta package containing all the most popular
;; SLIME contrib packages
(slime-setup '(slime-fancy))

(add-hook 'lisp-mode-hook 'lisp-coding-hook)
(add-hook 'slime-repl-mode-hook 'interactive-lisp-coding-hook)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(provide 'common-lisp-config)
