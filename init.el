;; Time-stamp: <2011-09-13 23:04:13 (bozhidar)>

;; Copyright (C) 2009-2011  Bozhidar Batsov.
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.

;; This is my personal Emacs setup, cleaned-up to be of some use to other
;; people as well. Hopefully, you'll find something of value for you
;; in it.

;; Emacs Dev Kit is targeting ONLY Emacs 23.x and Emacs 24.x. Older Emacs
;; versions are not supported.

;; a large part of Common Lisp implemented in Emacs Lisp
(require 'cl)

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(tool-bar-mode -1)
;; removing the menubar under OS X doesn't make much sense
(unless (string= system-type "darwin")
 (menu-bar-mode -1))
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; a proper stack trace is priceless
(setq debug-on-error t)

;; show a scrollbar on the right
(scroll-bar-mode t)
(set-scroll-bar-mode 'right)

;; on OS X Emacs doesn't use the system PATH for some reason
;; if you're using homebrew modifying the PATH is essential
(if (string= system-type "darwin")
    (push "/usr/local/bin" exec-path))

;; determine the load path dirs
;; as relative to the location of this file
(defvar dotfiles-dir "~/.emacs.d/"
  "The root Emacs Lisp source folder")

;; external packages reside here
(defvar ext-dir (concat dotfiles-dir "vendor/")
  "The root folder for external packages")

;; add everything to the load pah
(add-to-list 'load-path dotfiles-dir)

(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the
Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; add the first lever subfolders automatically
(add-subfolders-to-load-path dotfiles-dir)
(add-subfolders-to-load-path ext-dir)

;; Emacs 24 color themes
(when (= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/")))

;; set an explicit file to customization created via the UI
(setq custom-file (concat dotfiles-dir "custom.el"))

;; a list of all configurations that must be loaded
(defvar configs '(elpa misc coding emacs-lisp common-lisp scheme c
                       python ruby coffee-script haskell scala
                       ibuffer erc auctex nxml org prolog bindings))
(defun require-config (config)
  (message "Loading %s..." config)
  (require config)
  (message "Loaded %s." config))

;; require all the configs automatically
(dolist (config configs)
  (require-config (intern (concatenate 'string
                                       (symbol-name config)
                                       "-config"))))

;; load misc utils
(require-config 'misc-utils)
;; load editing utils
(require-config 'editing-utils)
;; load navigation utils
(require-config 'navigation-utils)
;; load coding utils - should be done before coding configs!
(require-config 'coding-utils)
;; load Mac OS X config if needed
(when (string= system-type "darwin")
  (require-config 'mac-config))

(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))

(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config)
    (load system-specific-config))
(if (file-exists-p user-specific-config)
    (load user-specific-config))
(if (file-exists-p user-specific-dir)
    (mapc #'load (directory-files user-specific-dir nil ".*el$")))
