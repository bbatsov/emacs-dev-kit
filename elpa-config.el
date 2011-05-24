;; Install a base set of packages automatically.
;;
;; Part of the Emacs Dev Kit

(defvar required-packages (list 'color-theme-zenburn
                                'gist
                                'magit                                  
                                'clojure-mode
                                'paredit
                                'yari
                                'prolog
                                'graphviz-dot-mode
                                'perlcritic
                                'yaml-mode)
  "Libraries that should be installed by default.")

(defun required-packages-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  ; create ELPA folder if it's not existing
  (unless (file-exists-p package-user-dir)
    (mkdir package-user-dir))
  (dolist (package required-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun online-p ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (online-p)
  (unless package-archive-contents (package-refresh-contents))
  (required-packages-install))

(provide 'elpa-config)
