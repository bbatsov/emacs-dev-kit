;; Time-stamp: <2011-05-19 23:34:48 (bozhidar)>

;; Copyright (C) 2009-2010  Bozhidar Batsov.
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.

;; A lot of the configuration here is stolen or inspired from someone
;; else's work. I thank all the people from which I have benefited and
;; I hope that many of you will benefit from me as well. :-)

;; a large part of Common Lisp implemented in Emacs Lisp
(require 'cl)

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(tool-bar-mode -1)
(menu-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; show a scrollbar on the right
(scroll-bar-mode t)
(set-scroll-bar-mode 'right)

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

;; set an explicit file to customization created via the UI
(setq custom-file (concat dotfiles-dir "custom.el"))

;; set the ELPA dir(packages downloaded by ELPA will go there)
(setq package-user-dir (concat dotfiles-dir "elpa"))

;; create the ELPA package dir if it doesn't exist
(if (not (file-exists-p package-user-dir))
    (mkdir package-user-dir))

;; load ELPA
(load "package")
(package-initialize)

;; add the marmalade package repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; a list of all configurations that must be loaded
(defvar configs '(elpa misc coding emacs-lisp common-lisp scheme clojure c
                       perl python java ruby scala haskell
                       ibuffer erc auctex nxml org ac bindings))

(defun require-config (config)
  (message "Loading %s..." config)
  (require config)
  (message "Loaded %s." config))

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
