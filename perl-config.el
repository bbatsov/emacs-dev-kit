;; cperl-mode - the best Perl mode for Emacs

;; use cperl-mode instead of perl-mode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; cperl-hairy affects all those variables, but I prefer
;; a more fine-grained approach as far as they are concerned
(setq cperl-font-lock t)
(setq cperl-electric-lbrace-space t)
(setq cperl-electric-parens nil)
(setq cperl-electric-linefeed nil)
(setq cperl-electric-keywords nil)
(setq cperl-info-on-command-no-prompt t)
(setq cperl-clobber-lisp-bindings t)
(setq cperl-lazy-help-time 3)

;; remove annoying trailing whitespace face(underscore)
(setq cperl-invalid-face nil)

(global-set-key (kbd "C-h P") 'perldoc)

(provide 'perl-config)
