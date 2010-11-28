;; cperl-mode - the best Perl mode for Emacs
(mapc                                            ;; use cperl-mode instead of perl-mode
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

(setq cperl-hairy t)                              ;; Turns on most of the cperl-mode options

(global-set-key (kbd "C-h P") 'perldoc)

(provide 'perl-config)
