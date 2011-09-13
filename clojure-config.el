;;; Clojure support
;;; This file is part of the Emacs Dev Kit

;; To start SLIME in your Clojure project:
;; 1. lein plugin install swank-clojure 1.3.1
;; 2. Invoke M-x clojure-jack-in from a project
(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'lisp-coding-hook)

(provide 'clojure-config)
