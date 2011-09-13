;;; Haskell
;;; This file is part of the Emacs Dev Kit

(load (concat ext-dir "haskell-mode-2.8.0/haskell-site-file"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'haskell-config)
