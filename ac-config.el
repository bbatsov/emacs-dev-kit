;; auto-complete-mode configuration
(require 'auto-complete-config)
(ac-config-default)

;; handy manual trigger for completion
;; if you're using a WM that does window switching with Alt+Tab
;; you can use the alternatives ESC TAB and C-M-i
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(provide 'ac-config)
