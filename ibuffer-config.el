;;; IBuffer
;;; This file is part of the Emacs Dev Kit

(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Org" ;; all org-related buffers
              (mode . org-mode))  
            ("Mail"
              (or  ;; mail-related buffers
               (mode . mew-message-mode)
               (mode . mew-summary-mode)
               (mode . mew-draft-mode)
               ))
            ("Programming" ;; prog stuff
              (or
                (mode . c-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)
                (mode . clojure-mode)
                )) 
            ("ERC"   (mode . erc-mode))
            ("Jabber"
             (or
              (mode . jabber-chat-mode)
              (mode . jabber-roster-mode)))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'ibuffer-config)
