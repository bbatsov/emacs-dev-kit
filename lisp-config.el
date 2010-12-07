;; some cool key bindings
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; Scheme - note that GDS is guile specific
;; and that it's bundled with Guile in recent versions
(require 'gds)

(setq scheme-program-name "guile")

;; a great lisp coding hook
(defun lisp-coding-hook ()
  (coding-hook)
  (setq autopair-dont-activate t)
  (paredit-mode +1))

;; interactive modes don't need whitespace checks
(defun interactive-lisp-coding-hook ()
  (setq autopair-dont-activate t)
  (paredit-mode +1)
  (turn-off-whitespace))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(add-hook 'emacs-lisp-mode-hook       'lisp-coding-hook)
(add-hook 'lisp-mode-hook             'lisp-coding-hook)
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (autopair-mode)
            (paredit-mode -1)
            (turn-off-whitespace)))
(add-hook 'ielm-mode-hook             'interactive-lisp-coding-hook)
(add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook           'lisp-coding-hook)

;; location of Common Lisp executable *not a folder*
(setq inferior-lisp-program "/usr/bin/clisp")
(require 'slime)
(slime-setup)
(slime-setup '(slime-fancy))

;; Clojure
(add-hook 'slime-repl-mode-hook       'interactive-lisp-coding-hook)
;(add-hook 'clojure-mode-hook 'lisp-coding-hook)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;(setq swank-clojure-extra-vm-args '("-Xms512m" "-Xmx1024m"))

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(provide 'lisp-config)
