;;; find-file-in-project.el --- Find files in a project quickly.

;; Copyright (C) 2006, 2007, 2008, 2009, 2010
;; Phil Hagelberg, Doug Alcorn, Will Farrington and Bozhidar Batsov

;; Author: Phil Hagelberg, Doug Alcorn, Will Farrington and Bozhidar Batsov
;; URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;; Git: git://github.com/wfarr/find-file-in-project.git
;; Version: 3.0
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library depends on GNU find.

;; This file provides a couple methods for quickly finding any file in
;; a given project. Projects are defined in two ways. The first uses
;; `locate-dominating-file'. First, if the `locate-dominating-file'
;; function is bound, it assumes you are using Emacs 23, in which case
;; you it will look for a `.dir-locals.el' file in an ancestor
;; directory of the current file. Otherwise it uses the
;;`project-local-variables' library, which looks for a `.emacs-project'
;; file.

;; The other method takes advantage of the prominence of version
;; control systems in projects to quickly identify the tree for a
;; project. It does so using `project.el' when available. `project.el'
;; is shipped in this tree, but for reasons of encouraging using
;; default Emacs behavior when and where possible, you will need to
;; manually require it in your Emacs configuration to make use of it.

;; By default, it looks only for files whose names match
;; `ffip-regexp', but it's understood that that variable will be
;; overridden locally. This can be done either with a mode hook:

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda (set (make-local-variable 'ffip-regexp) ".*\\.el")))

;; or by setting it in your .emacs-project/.dir-settings.el file, in
;; which case it will get set locally.

;; You can also be a bit more specific about what files you want to
;; find. For instance, in a Ruby on Rails project, you may be
;; interested in all .rb files that don't exist in the "vendor"
;; directory. In that case you could locally set `ffip-find-options'
;; to "" from within a hook or your .emacs-project file. The options
;; accepted in that variable are passed directly to the Unix `find'
;; command, so any valid arguments for that program are acceptable.

;; If `ido-mode' is enabled, the menu will use `ido-completing-read'
;; instead of `completing-read'.

;; Recommended binding:
;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

;;; TODO:

;; Performance testing with large projects
;; Switch to using a hash table if it's too slow
;; Add compatibility with BSD find (PDI; I can't virtualize OS X)

;;; Code:

(defvar ffip-ignored-folders
  '(".git" ".hg" ".bzr" ".svn" "CVS")
  "List of globally(for every project) ignored folders -
mostly VCS related(Git, SVN, etc.)")

(defvar ffip-project-ignored-folders
  nil
  "A list of all directories in the project to be ignored.
It should be set in .dir-locals.el in the root of the project.
The folders listed there will be added to the ones in the list
`ffip-ignored-folders'")

(defvar ffip-patterns
  nil
  "List of patterns to look for with find-file-in-project.")

(defvar ffip-exclude-patterns
  '("*.class" "*.*~")
  "List of patterns to ignore with find-file-in-project")

(defvar ffip-find-options
  ""
  "Extra options to pass to `find' when using find-file-in-project.

Use this to exclude portions of your project: \"-not -regex \\\".*vendor.*\\\"\"")

(defvar ffip-project-root nil
  "If non-nil, overrides the project root directory location.")

(defvar ffip-top-level-project-files
  '(".dir-locals.el" ".git" ".hg" ".bzr" ".emacs-project" "_darcs" ".ensime")
  "What file should ffip look for to define a project?")

(defvar ffip-other-project-files
  '(".svn" "CVS" "RCS"))

(defconst ffip-gnu-find-command-string
  "find %s -type d \\( %s \\) -prune -o -type f \\( %s %s \\) %s -print"
  "GNU find command string")

(defun ffip-project-files ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (file-name-nondirectory file)
                                     (expand-file-name file))))
                (when (assoc (car file-cons) file-alist)
                  (ffip-uniqueify (assoc (car file-cons) file-alist))
                  (ffip-uniqueify file-cons))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string
                           (let
                               ((command-string
                                 (format ffip-gnu-find-command-string
                                         (or ffip-project-root
                                             (ffip-project-root)
                                             (error "no project root found"))
                                         (ffip-join-ignored-folders)
                                         (ffip-join-patterns)
                                         (if ffip-patterns
                                             (concat " -a " (ffip-join-excluded-patterns))
                                           (ffip-join-excluded-patterns))
                                         ffip-find-options)))
                             (message command-string)
                             command-string))))))

;; TODO: Emacs has some built-in uniqueify functions; investigate using those.
(defun ffip-uniqueify (file-cons)
  "Set the car of the argument to include the directory name plus the file name."
  (setcar file-cons
          (concat (car file-cons) ": "
                  (cadr (reverse (split-string (cdr file-cons) "/"))))))

(defun ffip-join-ignored-folders ()
  "Turn `ffip-ignored-folders' into a string that `find' can use."
  (mapconcat (lambda (pat) (format "-name %s" pat))
             ;; combine the global ignore list with the folders ignored
             ;; in the project(if any)
             (append ffip-ignored-folders
                     ffip-project-ignored-folders)
             " -o "))

(defun ffip-join-patterns ()
  "Turn `ffip-paterns' into a string that `find' can use."
  (if ffip-patterns
      (mapconcat (lambda (pat) (format "-name \"%s\"" pat))
                 ffip-patterns " -o ")
    "")
  )

(defun ffip-join-excluded-patterns ()
  "Turn `ffip-exclude-paterns' into a string that `find' can use."
  (if ffip-exclude-patterns
      (mapconcat (lambda (pat) (format "-not -name \"%s\"" pat))
                 ffip-exclude-patterns " -a ")
    "")
  )

;;;###autoload
(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
any of the files in the `.ffip-project-files' list. You can
override this by locally setting the `ffip-project-root' variable."
  (interactive)
  (let* ((project-files (ffip-project-files))
         (file (if (and (boundp 'ido-mode) ido-mode)
                   (ido-completing-read "Find file in project: "
                                        (mapcar 'car project-files))
                 (completing-read "Find file in project: "
                                  (mapcar 'car project-files)))))
    (find-file (cdr (assoc file project-files)))))

(defun ffip-project-root ()
  "Return the root of the project.

If `locate-dominating-file' is bound we use it directly - otherwise we use
a replacement for it. A list of project files is search for - the first
match is used to determine the root of the project."

  (let ((root (project-root)))
    (dolist (file ffip-other-project-files)
     (message "Checking for file %s " file)
     (if (file-exists-p (concat default-directory file))
         (setq root (project-root-alternative default-directory file))))
    root))

(defun project-root ()
  (loop for file in ffip-top-level-project-files
        when (ffip-locate-dominating-file default-directory file)
        do (return it)))

(defun project-root-alternative (dir file)
  (while (file-exists-p (concat (file-name-directory dir) file))
    (message "Searching dir %s" dir)
    (setq dir (directory-file-name (file-name-directory dir))))
  dir)

;; Backport functionality to Emacs 22
(if (functionp 'locate-dominating-file)
    (defalias 'ffip-locate-dominating-file 'locate-dominating-file)
  (defun ffip-locate-dominating-file (file name)
    "Look up the project file in and above `file'."
    (let ((parent (file-truename (expand-file-name ".." file))))
      (cond ((string= file parent) nil)
            ((file-exists-p (concat file name)) file)
            (t (plv-find-project-file parent name))))))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
