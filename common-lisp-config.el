;;; Common Lisp
;;; This file is part of the Emacs Dev Kit

;; a list of alternative Common Lisp implementations that can be
;; used with SLIME. Note that their presence render
;; inferior-lisp-program useless. This variable holds a list of
;; programs and if you invoke SLIME with a negative prefix
;; argument, M-- M-x slime, you can select a program from that list.
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
        (clisp ("clisp" "-q"))
        (cmucl ("cmucl" "-quiet"))))

;; delay SLIME loading until you actually need it
(require 'slime-autoloads)

;; slime-fancy is meta package containing all the most popular
;; SLIME contrib packages
(slime-setup '(slime-fancy))

(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-fuzzy-completion-in-place t
      slime-enable-evaluate-in-emacs t
      slime-autodoc-use-multiline-p t)

(add-hook 'lisp-mode-hook 'lisp-coding-hook)
(add-hook 'slime-repl-mode-hook 'interactive-lisp-coding-hook)

;; start slime automatically when we open a lisp file
(defun start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'start-slime)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
(define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
(define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)

(provide 'common-lisp-config)
