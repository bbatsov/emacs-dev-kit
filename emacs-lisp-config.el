;;; Emacs Lisp
;;; This file is part of the Emacs Dev Kit

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(add-hook 'emacs-lisp-mode-hook 'lisp-coding-hook)

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (when (= emacs-major-version 23)
              (autopair-mode))
            (paredit-mode -1)
            (turn-off-whitespace)))
(add-hook 'ielm-mode-hook 'interactive-lisp-coding-hook)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(provide 'emacs-lisp-config)
