;; user malabar-mode for Java development
(add-to-list 'load-path (concat ext-dir "malabar-1.5-SNAPSHOT/lisp"))

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir (concat ext-dir "malabar-1.5-SNAPSHOT/lib"))
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; enable camel case aware editing
(add-hook 'malabar-mode-hook (lambda () (subword-mode)))

(provide 'java-config)
