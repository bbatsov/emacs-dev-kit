;;; nav.el --- Emacs mode for IDE-like navigation of directories
;;
;; Copyright 2009 Google Inc. All Rights Reserved.
;;
;; Author: issactrotts@google.com (Issac Trotts)
;; Version: 20090823
;;

;;; License:
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; 
;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/nav/")
;; (require 'nav)
;;
;; Type M-x nav to open the navigation window. It should show up as a
;; narrow column on the left, showing the contents of the current
;; directory. If there are multiple windows open, all but one will be
;; closed to make sure the nav window shows up correctly.

;;; Key Bindings
;;
;; Press ? in the Nav window to display a list of key bindings.
;;

;;; History:
;;
;; See http://code.google.com/p/emacs-nav/source/list
;;

;;; Code:

(require 'cl)
(require 'nav-bufs)
(require 'nav-tags)
(require 'nav-preview)

(defconst nav-max-int 268435455)

(defgroup nav nil
  "A lightweight file/directory navigator."
  :group 'applications)

(defcustom nav-default-width 20
  "*Initial width of the Nav window."
  :type 'integer
  :group 'nav)

(defcustom nav-quickdir-list
  (list "~" "~/.emacs.d" "/tmp")
  "*Nav bookmark list. Fill this with your most frequently visited directories."
  :type '(repeat string)
  :group 'nav)

(defcustom nav-quickfile-list
  (list "~/.emacs.d/nav.el" "~/.emacs.d/nav-bufs.el" "~/.emacs.d/nav-tags.el")
  "*Nav quick file list. Fill this with your most frequently visited files."
  :type '(repeat string)
  :group 'nav)

(defcustom nav-follow nil
  "*If t, nav will follow buffer's directory."
  :type 'boolean
  :group 'nav)

(defcustom nav-hidden nil
  "*If t, nav will show hidden files and directories."
  :type 'boolean
  :group 'nav)

(defcustom nav-follow-delay 0.05
  "*How long Nav waits before checking to see if the directory has changed.
Nav must be restarted for changes to this variable to take effect."
  :type 'float
  :group 'nav)

(defcustom nav-quickjump-show t
  "*If t, nav will show quickjump buttons."
  :type 'boolean
  :group 'nav)

(defcustom nav-no-hidden-boring-file-regexps
  (list "\\.pyc$" "\\.o$" "~$" "\\.bak$"
        "^\\.*/?$")                     ; ./ and ../
  "*In no-hidden mode, nav ignores filenames matching any regex in this list."
  :type '(repeat string)
  :group 'nav)

(defcustom nav-boring-file-regexps
  (append nav-no-hidden-boring-file-regexps 
          (list "^\\.[^/]"          ; files such as .foo
                "/\\."))            ; any path with a hidden component
  "*Nav ignores filenames that match any regular expression in this list."
  :type '(repeat string)
  :group 'nav)

(defcustom nav-split-window-direction 'horizontal
  "*Window split direction for `nav-open-file-other-window-2'.

This is used if only one window besides the Nav window is visible."
  :type '(choice (const horizontal) (const vertical))
  :group 'nav)

(defcustom nav-resize-frame-p nil
  "*If non-nil, activating and deactivating nav will resize the current frame."
  :type 'boolean
  :group 'nav)

(defcustom nav-widths-percentile 90
  "*What percentage of files should remain completely visible when shrink-wrapping."
  :type 'integer
  :group 'nav)

;; Make nav faces
(make-empty-face 'nav-face-heading)
(make-empty-face 'nav-face-button-num)
(make-empty-face 'nav-face-dir)
(make-empty-face 'nav-face-hdir)
(make-empty-face 'nav-face-file)
(make-empty-face 'nav-face-hfile)

;; params here are: foreground background stipple bold ital underline inverse
;; (modify-face 'nav-face-heading "white" "navy" nil nil nil nil nil)
;; (modify-face 'nav-face-button-num "#8722c9" nil nil nil nil nil nil)
;; (modify-face 'nav-face-dir "ForestGreen" nil nil nil nil nil nil)
;; (modify-face 'nav-face-hdir "Red1" nil nil nil nil nil nil)
;; (modify-face 'nav-face-file nil nil nil nil nil nil nil)
;; (modify-face 'nav-face-hfile "pink" nil nil nil nil nil nil)

(setq nav-face-heading 'nav-face-heading)
(setq nav-face-button-num 'nav-face-button-num)
(setq nav-face-dir 'nav-face-dir)
(setq nav-face-hdir 'nav-face-hdir)
(setq nav-face-file 'nav-face-file)
(setq nav-face-hfile 'nav-face-hfile)


(defun nav-insert-text (text face-type)
  "Inserts text with select face."
  (interactive)
  (insert text)
  (overlay-put
   (make-overlay (line-beginning-position) (line-end-position))
   'face face-type))


(defun nav-buffer-menu-window-1 ()
  (interactive)
  (other-window 1)
  (buffer-menu))


(defun nav-buffer-menu-window-2 ()
  (interactive)
  (nav-ensure-second-window-exists)
  (other-window 2)
  (buffer-menu))


(defun nav-make-mode-map ()
  "Creates and returns a mode map with nav's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\n" 'nav-open-file-under-cursor)
    (define-key keymap "\r" 'nav-open-file-under-cursor)
    (define-key keymap "1" 'nav-open-file-other-window-1)
    (define-key keymap "2" 'nav-open-file-other-window-2)
    (define-key keymap "5" (lambda nil (interactive) (nav-quickfile-jump 0)))
    (define-key keymap "6" (lambda nil (interactive) (nav-quickfile-jump 1)))
    (define-key keymap "7" (lambda nil (interactive) (nav-quickfile-jump 2)))
    (define-key keymap "8" (lambda nil (interactive) (nav-quickdir-jump 0)))   
    (define-key keymap "9" (lambda nil (interactive) (nav-quickdir-jump 1)))
    (define-key keymap "0" (lambda nil (interactive) (nav-quickdir-jump 2)))
    (define-key keymap "a" 'nav-make-new-file)
    (define-key keymap "b" 'nav-show-bufs)
    (define-key keymap "c" 'nav-copy-file-or-dir)
    (define-key keymap "C" 'nav-customize)
    (define-key keymap "d" 'nav-delete-file-or-dir-on-this-line)
    (define-key keymap "e" 'nav-invoke-dired)  
    (define-key keymap "f" 'nav-find-files)
    (define-key keymap "F" 'nav-toggle-follow)
    (define-key keymap "g" 'nav-recursive-grep)
    (define-key keymap "h" 'nav-jump-to-home)
    (define-key keymap "j" 'nav-jump-to-dir)
    (define-key keymap "m" 'nav-move-file-or-dir)
    (define-key keymap "n" 'nav-make-new-directory)
    (define-key keymap "p" 'nav-pop-dir)
    (define-key keymap "P" 'nav-print-current-dir)
    (define-key keymap "o" (lambda nil (interactive) (other-window 1)))
    (define-key keymap "q" 'nav-quit)
    (define-key keymap "r" 'nav-refresh)
    (define-key keymap "s" 'nav-shell)
    (define-key keymap "t" 'nav-tags-expand)
    (define-key keymap "u" 'nav-go-up-one-dir)
    (define-key keymap "v" 'nav-view-file)
    (define-key keymap "V" 'nav-toggle-preview)
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "W" 'nav-set-width-to-default)
    (define-key keymap "[" 'nav-rotate-windows-ccw)
    (define-key keymap "]" 'nav-rotate-windows-cw)
    (define-key keymap "!" 'nav-shell-command)
    (define-key keymap "." 'nav-toggle-hidden-files)
    (define-key keymap "?" 'nav-help-screen)
    (define-key keymap " " 'nav-jump-to-name)
    (define-key keymap [S-down-mouse-3] 'nav-mouse-tags-expand)
    (define-key keymap [mouse-3] 'nav-show-bufs)

    ;; Avoid [(tab)] and [(shift tab)] because they fail on Issac's setup.
    (define-key keymap "\t" 'forward-button)
    (define-key keymap [backtab] 'backward-button)

    (define-key keymap [(down)] 'forward-button)
    (define-key keymap [(up)] 'backward-button)
    (define-key keymap [(control ?n)] 'forward-button)
    (define-key keymap [(control ?p)] 'backward-button)
    (define-key keymap [(control ?x) (control ?f)] 'find-file-other-window)
    keymap))


;; I use setq instead of defvar here so we can just use M-x
;; eval-buffer instead of restarting emacs or other junk after
;; changing the nav mode map.
(setq nav-mode-map (nav-make-mode-map))

(defvar nav-preview nil)

(defvar nav-follow-timer nil
  "Timer used to update Nav's contents to reflect the directory
of the current buffer")

(defvar nav-width nav-default-width)

(defvar nav-dir-stack '())

(defvar nav-map-dir-to-line-number (make-hash-table :test 'equal)
  "Hash table from dir paths to most recent cursor pos in them.")

(defvar nav-filter-regexps nav-boring-file-regexps)

(defvar nav-button-face nil)

(defvar nav-other-width nav-width
  "Used for toggling on w key.")

(defconst nav-shell-buffer-name "*nav-shell*"
  "Name of the buffer used for the command line shell spawned by
  nav on the 's' key.")

(defconst nav-buffer-name "*nav*"
  "Name of the buffer where nav shows directory contents.")

(defconst nav-buffer-name-for-find-results "*nav-find*"
  "Name of the buffer where nav shows results of its find command ('f' key).")

(define-button-type 'quickdir-jump-button
  'action 'nav-quickdir-jump-button-action
  'follow-link t
  'face nil
  'help-echo nil)

(define-button-type 'quickfile-jump-button
  'action 'nav-quickfile-jump-button-action
  'follow-link t
  'face nil
  'help-echo nil)

(defun nav-show-bufs ()
  (interactive)
  (nav-cancel-timer)
  (nav-bufs))


(defun nav-start-timer ()
  "Starts the timer to update Nav while following."
  (setq nav-follow-timer
        (run-with-idle-timer nav-follow-delay t 'nav-follow-buffer)))


(defun nav-cancel-timer ()
  "Cancels Nav's timer and sets it to nil."
  (when nav-follow-timer
    (cancel-timer nav-follow-timer)
    (setq nav-follow-timer nil)))


(defun turn-off-font-lock ()
  (font-lock-mode -1))


(defun nav-join (sep string-list)
  (mapconcat 'identity string-list sep))


(defun nav-toggle-hidden-files ()
  (interactive) 
  (if (equal nav-filter-regexps nav-boring-file-regexps)
      (progn
	(setq nav-filter-regexps nav-no-hidden-boring-file-regexps)
	(setq nav-hidden t))
    (progn
      (setq nav-filter-regexps nav-boring-file-regexps)
      (setq nav-hidden nil)))
  (nav-show-dir "."))


(defun nav-filename-matches-some-regexp (filename regexps)
  (let ((matches-p nil))
    (dolist (rx regexps)
      (if (string-match rx filename)
          (setq matches-p t)))
      matches-p))


(defun nav-filter-out-boring-filenames (filenames boring-regexps)
  (flet ((is-boring (filename)
                    (nav-filename-matches-some-regexp filename boring-regexps)))
    (remove-if 'is-boring filenames)))


(defun nav-get-non-boring-filenames-recursively (dirname)
  (let ((paths (nav-get-paths dirname)))
    (nav-filter-out-boring-filenames paths (cons "/$" nav-filter-regexps))))


(defun nav-dir-files-or-nil (dirname)
  "Returns a list of files in DIRNAME. 
If DIRNAME is not a directory or is not accessible, returns nil."
  (condition-case err
      (directory-files dirname)
    (file-error nil)))


(defun nav-get-line-for-cur-dir ()
  (gethash (nav-get-working-dir) nav-map-dir-to-line-number))


(defun nav-cd (dirname)
  "Changes to a different directory and pushes it onto the stack."
  (let ((dirname (file-name-as-directory (file-truename dirname))))
    (nav-save-cursor-line)
    (setq default-directory dirname)
    (nav-show-dir dirname)
    (nav-restore-cursor-line)))


(defun nav-save-cursor-line ()
  "Updates line number hash table."
  (let ((line-num (nav-line-number-at-pos (point))))
    (puthash (nav-get-working-dir) line-num nav-map-dir-to-line-number)))


(defun nav-restore-cursor-line ()
  "Remembers what line we were on last time we visited this directory."
  (let ((line-num (nav-get-line-for-cur-dir)))
    (goto-line (if line-num line-num 3))))


(defun nav-open-file (filename)
  "Opens a file or directory from Nav."
  (interactive "FFilename:")
  (if (file-directory-p filename)
      (nav-push-dir filename)
    (find-file-other-window filename)))


(defun nav-open-file-under-cursor ()
  "Finds the file undert the cursor in the window not containing Nav."
  (interactive)
  (let ((filename (nav-get-cur-line-str)))
    (nav-open-file filename)))


(defun nav-go-up-one-dir ()
  "Points Nav to ../."
  (interactive)
  (nav-push-dir ".."))


(defun nav-shrink-wrap ()
  "Updates the width of the Nav window to fit the longest filename in the
current directory. Updates the global variable nav-width as a side effect."
  (interactive)
  (let* ((lines (split-string (buffer-string) "\n" t))
	 (num-lines (length lines))
	 (line-lengths (mapcar 'length lines))
	 (desired-width (+ 1 (nav-percentile nav-widths-percentile
					     (sort line-lengths '<))))
	 (max-width (/ (frame-width) 2))
	 (new-width (min desired-width max-width)))
    (setq nav-width new-width)
    (nav-set-window-width new-width)))


(defun nav-percentile (percent sorted-things)
  "Returns the item a certain percent of the way through a list of items
assumed to be sorted."
  (let* ((n (length sorted-things))
	 (k (min (- n 1 )
		 (truncate (* (/ percent 100.0) n)))))
    (nth k sorted-things)))


(defun nav-set-width-to-default ()
  "Sets the width of the Nav window to nav-default-width, and
updates the nav-width global variable as a side effect."
  (interactive)
  (setq nav-width nav-default-width)
  (nav-set-window-width nav-width))


(defun nav-push-dir (dirname)
  (let ((dirname (file-truename dirname)))
    (when (not (string= dirname default-directory))
      (push (file-truename default-directory) nav-dir-stack)
      (nav-cd dirname))))


(defun nav-pop-dir ()
  "Goes to the previous directory in Nav's history.
This works like a web browser's back button."
  (interactive)
  (let ((dir nil))
    (while (and nav-dir-stack
                (or (not dir)
                    (equal dir (file-name-as-directory (file-truename ".")))
                    (not (file-exists-p dir))))
      (setq dir (pop nav-dir-stack)))
    (setq dir (or dir "."))
    (nav-cd dir)))


(defun nav-get-cur-line-str ()
  (let ((str (buffer-substring-no-properties (point-at-bol)
					     (point-at-eol))))
    ;; Filter out button junk such as [5], [6], and so on.
    (replace-regexp-in-string "^[0-9]. " "" str)))


(defun nav-non-boring-directory-files (dir)
  (nav-filter-out-boring-filenames (directory-files dir) nav-filter-regexps))


(defun nav-dir-suffix (dir)
  (replace-regexp-in-string ".*/" "" (directory-file-name dir)))


(defun nav-line-number-at-pos (p)
  (let ((line-num 1))
    (dotimes (i p line-num)
      (if (eq ?\n (char-after i))
          (setq line-num (+ line-num 1))))))


(defun nav-quickdir-jump-button-action (button)
  (let ((num (string-to-number (substring (button-label button) 0 1))))
    (if (= num 0) (setq num 2))
    (if (= num 9) (setq num 1))
    (if (= num 8) (setq num 0))
    (nav-quickdir-jump num)))


(defun nav-quickfile-jump-button-action (button)
  (select-window (nav-get-window nav-buffer-name))
  (let* ((num (string-to-number (substring (button-label button) 0 1)))
         (num (- num 5)))
    (nav-quickfile-jump num)))


(defun nav-replace-buffer-contents (new-contents should-make-filenames-clickable)
  (let ((saved-line-number (nav-line-number-at-pos (point)))
        ;; Setting inhibit-read-only to t here lets us edit the buffer
        ;; in this let-block.
        (inhibit-read-only t))
    (erase-buffer)
    (if should-make-filenames-clickable
	(progn
	  (nav-insert-text "Directory:" nav-face-heading)
	  (insert "\n")))
    (insert new-contents)
    (if should-make-filenames-clickable
	(progn
	  (nav-make-filenames-clickable)
          (nav-colorize-filenames)
	  (if nav-quickjump-show (nav-insert-jump-buttons))))
    (goto-line saved-line-number)))


(defun nav-insert-jump-buttons ()
  ;; Make quickjump buttons.
  (insert "\n\n")
  (nav-insert-text "Quickjumps:" nav-face-heading)
  (insert "\n")
  (setq qfilename (replace-regexp-in-string "^.*/" "" (nth 0 nav-quickfile-list)))
  (insert-text-button (concat (propertize "5." 'face nav-face-button-num) " "
			      (propertize qfilename 'face nav-face-file))
		      :type 'quickfile-jump-button)
  (insert "\n")
  (setq qfilename (replace-regexp-in-string "^.*/" "" (nth 1 nav-quickfile-list)))
  (insert-text-button (concat (propertize "6." 'face nav-face-button-num) " "
			      (propertize qfilename 'face nav-face-file)) 
		      :type 'quickfile-jump-button)
  (insert "\n")
  (setq qfilename (replace-regexp-in-string "^.*/" "" (nth 2 nav-quickfile-list)))
  (insert-text-button (concat (propertize "7." 'face nav-face-button-num) " "
			      (propertize qfilename 'face nav-face-file)) 
		      :type 'quickfile-jump-button)
  (insert "\n")
  (insert-text-button (concat (propertize "8." 'face nav-face-button-num) " " 
			      (propertize (nth 0 nav-quickdir-list) 'face nav-face-dir))
		      :type 'quickdir-jump-button)
  (insert "\n")
  (insert-text-button (concat (propertize "9." 'face nav-face-button-num) " "  
			      (propertize (nth 1 nav-quickdir-list) 'face nav-face-dir))
		      :type 'quickdir-jump-button)
  (insert "\n")
  (insert-text-button (concat (propertize "0." 'face nav-face-button-num) " "  
			      (propertize (nth 2 nav-quickdir-list) 'face nav-face-dir))
		      :type 'quickdir-jump-button))


(defun nav-make-filenames-clickable ()
  (condition-case err
      (save-excursion
	(goto-line 2)
	(dotimes (i (count-lines 1 (point-max)))
	  (let ((start (line-beginning-position))
		(end (line-end-position)))
	    (make-button start end
			 'action (lambda (button)
				   (select-window (nav-get-window nav-buffer-name))
				   (nav-open-file (button-label button)))
			 'follow-link t
			 'face nav-button-face
			 'help-echo nil))
	  (forward-line 1)))
    (error 
     ;; This can happen for versions of emacs that don't have
     ;; make-button defined.
     'failed)))


(defun nav-colorize-filenames()
  "Adds faces to the directory listing."
	(goto-line 2)
	(dotimes (i (count-lines 1 (point-max)))
	  (let ((start (line-beginning-position))
		(end (line-end-position))
		(filename (buffer-substring (line-beginning-position) (line-end-position)))
		(face-type nav-face-file))
	    (if (looking-at "^[.].*")(setq face-type nav-face-hfile))
	    (if (looking-at "^.*/$")(setq face-type nav-face-dir))
	    (if (looking-at "^[.].*/$")(setq face-type nav-face-hdir))
	    (overlay-put 
	     (make-overlay start end)
	     'face face-type)
	    (forward-line 1))))


(defun nav-string< (s1 s2)
  "Tells whether S1 comes lexically before S2, ignoring case."
  (string< (downcase s1) (downcase s2)))


(defun nav-show-dir (dir)
  (let ((new-contents '()))
    (dolist (filename (nav-non-boring-directory-files dir))
      (let ((line (concat "\n" filename
                          (if (file-directory-p filename)
                              "/"
                            ""))))
        (push line new-contents)))
    (let* ((new-contents (sort new-contents 'nav-string<))
           (new-contents (nav-join "" (cons "../" new-contents))))
      (nav-replace-buffer-contents new-contents t))
    (setq mode-line-format (nav-update-mode-line "d" dir))
    (force-mode-line-update)))


(defun nav-set-window-width (n)
  (if (> (window-width) n)
    (shrink-window-horizontally (- (window-width) n)))
  (if (< (window-width) n)
    (enlarge-window-horizontally (- n (window-width)))))


(defun nav-set-window-height (n)
  (if (> (window-height) n)
    (shrink-window (- (window-height) n)))
  (if (< (window-height) n)
    (enlarge-window (- n (window-height)))))


(defun nav-get-working-dir ()
  (save-current-buffer
    (set-buffer nav-buffer-name)
    (file-name-as-directory (file-truename default-directory))))


(defun nav-invoke-dired ()
  "Invokes dired on the current directory."
  (interactive)
  (other-window 1)
  (dired (nav-get-working-dir)))


(defun nav-open-file-other-window (k)
  (let ((filename (nav-get-cur-line-str))
        (dirname (nav-get-working-dir)))
    (other-window k)
    (find-file (concat dirname "/" filename))))


(defun nav-open-file-other-window-1 ()
  "Opens the file under the cursor in the first other window.

This is equivalent to just pressing the [enter] key. 
See nav-open-file-other-window-2."
  (interactive)
  (nav-open-file-other-window 1))


(defun nav-ensure-second-window-exists ()
  "Makes sure there is a second file-editing area on the right.

Jumps back to nav window when done."
  (when (= 2 (length (window-list)))
    (other-window 1)
    (if (eq nav-split-window-direction 'horizontal)
        (split-window-horizontally)
      (split-window-vertically))
    (select-window (nav-get-window nav-buffer-name))))


(defun nav-open-file-other-window-2 ()
  "Opens the file under the cursor in the second other window.

If there is no second other window, Nav will create one."
  (interactive)
  (nav-ensure-second-window-exists)
  (nav-open-file-other-window 2))


(defun nav-get-window (buf-name)
  "Returns a window whose buffer has a given name."
  (let ((nav-win nil))
    (dolist (w (window-list))
      (if (string= buf-name (buffer-name (window-buffer w)))
          (setq nav-win w)))
    nav-win))


(defun nav-outer-width ()
  (let* ((edges (window-edges (nav-get-window nav-buffer-name)))
         (left (nth 0 edges))
         (right (nth 2 edges)))
    (- right left)))


(defun nav-refresh ()
  "Resizes Nav window to original size, updates its contents."
  (interactive)
  (nav-set-window-width nav-width)
  (nav-show-dir ".")
  (nav-restore-cursor-line))


(defun nav-equalize-window-widths ()
  "Equalizes the widths of top-level windows if top split is horizontal."
  (interactive)
  (let ((root (car (window-tree))))
    (when (listp root)
      ;; The root window is split.
      (let ((split-is-vertical (car root)))
        (when (not split-is-vertical)
          (let* ((edges (cadr root))
                 (left (nth 0 edges))
                 (right (nth 2 edges))
                 (total-width (+ (- right left) 1))
                 (top-windows (cddr root))
                 (num-windows (length top-windows))
                 (avg-width (/ total-width num-windows))
                 (saved-window (selected-window)))
            (dolist (window top-windows)
              (when (window-live-p window)
                (select-window window)
                (nav-set-window-width avg-width)))
            (select-window saved-window)))))))


(defun nav-quit ()
  "Exits Nav."
  (interactive)
  (nav-cancel-timer)
  (let ((window (get-buffer-window nav-buffer-name)))
    (when window
      (let ((this-is-not-the-only-window (not (equal window (next-window window)))))
	(when this-is-not-the-only-window
	  (when nav-resize-frame-p
	    (set-frame-width (selected-frame) 
			     (- (frame-width) (nav-outer-width))))
	  (delete-window window)))))
  (kill-buffer nav-buffer-name)
  (nav-equalize-window-widths))


(defun nav-is-open ()
  "Returns non-nil if Nav is open."
  (nav-get-window nav-buffer-name))


(defun nav-toggle ()
  "Toggles whether Nav is active.

Synonymous with the (nav) function."
  (interactive)
  (nav))


(defun nav-make-recursive-grep-command (pattern)
  (let* ((file-paths (nav-get-non-boring-filenames-recursively "."))
         (temp-filename (make-temp-file "nav")))
    (other-window 1)
    (save-current-buffer
      (find-file temp-filename)
      (make-local-variable 'require-final-newline)
      (setq require-final-newline nil)
      (insert (nav-join "\0" file-paths))
      (save-buffer)
      (kill-buffer (current-buffer)))
    (select-window (nav-get-window nav-buffer-name))
    (let ((pattern (nav-escape-single-quotes pattern)))
      (format "cat '%s' | xargs -0 grep -inH --regexp='%s'" temp-filename pattern))))


(defun nav-escape-single-quotes (string)
  "Replaces ' with '\'' in a string, for use inside a single quoted string that will
be interpreted by bash.

http://muffinresearch.co.uk/archives/2007/01/30/bash-single-quotes-inside-of-single-quoted-strings/
"
  (replace-regexp-in-string "'" "'\\\\''" string))


(defun nav-recursive-grep (pattern)
  "Greps for a regular expression in '.' and all subdirectories."
  (interactive "sPattern to recursively grep for: ")
  (grep (nav-make-recursive-grep-command pattern))
  (other-window 1))


(defun nav-toggle-follow ()
  "Toggle nav follow."
  (interactive)
  (if (not nav-follow)
      (progn
        (nav-start-timer)
	(setq nav-follow t))
    (progn
      (nav-cancel-timer)
      (setq nav-follow nil)))
  (nav-refresh))
    

(defun nav-jump-to-home ()
  "Show home directory in Nav."
  (interactive)
  (nav-push-dir "~"))


(defun nav-jump-to-name (arg)
  (interactive "K")
  (goto-line 2)
  (let ((nav-search-string (concat "^" arg)))
    (search-forward-regexp nav-search-string)))


(defun nav-quickfile-jump (quickfile-num)
  "Jumps to directory from custom bookmark list."
  (interactive)
  (let* ((filename (nth quickfile-num nav-quickfile-list))
	 (filename (substitute-in-file-name filename)))
    (nav-open-file filename)))


(defun nav-quickdir-jump (quickdir-num)
  "Jumps to directory from custom bookmark list."
  (interactive)
  (let* ((dirname (nth quickdir-num nav-quickdir-list))
	 (dirname (substitute-in-file-name dirname)))
    (nav-push-dir dirname)))


(defun nav-jump-to-dir (dirname)
  "Shows a specified directory in Nav."
  (interactive "fDirectory: ")
  (nav-push-dir dirname))


(defun nav-update-mode-line (mode dir)
  (setq nav-mode-line (concat "-(nav)" 
			      (if nav-hidden 
				  (format "%s" "H")
				(format "%s" "-"))  
			      (if nav-follow 
				  (format "%s" "F")
				(format "%s" "-")) 
			      (if nav-preview
				  (format "%s" "P")
				(format "%s" "-")) 
			      " "
			      (if (string= mode "d")
				  (propertize (concat (nav-dir-suffix (file-truename dir)) "/")
					      'face 'modeline-buffer-id))
			      (if (string= mode "b")
				  (propertize "Buffer Mode" 'face 'modeline-buffer-id))
		      (if (string= mode "t")
			  (propertize "Tag Mode" 'face 'modeline-buffer-id))))
  nav-mode-line)


(defun nav-this-is-a-microsoft-os ()
  (or (string= system-type "windows-nt")
      (string= system-type "ms-dos")))


(defun nav-make-remove-dir-command (dirname)
  (if (nav-this-is-a-microsoft-os)
      (format "rmdir /S /Q \"%s\"" dirname)
    (format "rm -rf '%s'" dirname)))


(defun nav-delete-file-or-dir (filename)
  (nav-save-cursor-line)
  (if (and (file-directory-p filename)
           (not (file-symlink-p (directory-file-name filename))))
      (when (yes-or-no-p (format "Really delete directory %s ?" filename))
        (shell-command (nav-make-remove-dir-command filename))
        (nav-refresh))
      ;; We first use directory-file-name to strip the trailing slash
      ;; if it's a symlink to a directory.
      (let ((filename (directory-file-name filename)))
        (when (y-or-n-p (format "Really delete file %s ? " filename))
          (delete-file filename)
          (nav-refresh))))
  (nav-restore-cursor-line))


(defun nav-delete-file-or-dir-on-this-line ()
  "Deletes a file or directory."
  (interactive)
  (nav-delete-file-or-dir (nav-get-cur-line-str)))


(defun nav-tags-expand ()
  "Shows all function tags in file."
  (interactive)
  (nav-cancel-timer)
  (nav-save-cursor-line)
  (let ((filename (nav-get-cur-line-str)))
    (nav-tags-fetch-imenu filename)))


(defun nav-mouse-tags-expand ()
  "Sets point to current mouse pos then calls nav-tags-expand."
  (interactive)
  (goto-line (+ 1  (cddr (mouse-position))))
  (nav-tags-expand))
  

(defun nav-ok-to-overwrite (target-name)
  "Returns non-nil if it's ok to overwrite or create a file.

That is, if a file with the given name doesn't exist, is a
directory, or if the user says it's ok."
  (or (not (file-exists-p target-name))
      (file-directory-p target-name)
      (y-or-n-p (format "Really overwrite %s ? " target-name))))


(defun nav-copy-file-or-dir (target-name)
  "Copies a file or directory."
  (interactive "FCopy to: ")
  (let ((filename (nav-get-cur-line-str)))
    (if (nav-this-is-a-microsoft-os)
        (copy-file filename target-name)
      (if (nav-ok-to-overwrite target-name)
          (let ((maybe-dash-r (if (file-directory-p filename) "-r" "")))
            (shell-command (format "cp %s '%s' '%s'" maybe-dash-r
                                   (expand-file-name filename)
                                   (expand-file-name target-name)))))))
  (nav-refresh))


(defun nav-customize ()
  "Starts customization for Nav."
  (interactive) 
  (other-window 1)
  (customize-group "nav"))


(defun nav-move-file-or-dir (target-name)
  "Moves a file or directory."
  (interactive "FMove to: ")
  (let ((filename (nav-get-cur-line-str)))
    (if (nav-this-is-a-microsoft-os)
        (rename-file filename target-name)
      (if (nav-ok-to-overwrite target-name)
          (shell-command (format "mv '%s' '%s'"
                                 (expand-file-name filename)
                                 (expand-file-name target-name))))))
  (nav-refresh))


(defun nav-make-grep-list-cmd (pattern filenames)
  (if (not filenames)
      ""
    (format "grep -il '%s' %s" pattern (nav-join " " filenames))))


(defun nav-append-slashes-to-dir-names (names)
  (mapcar (lambda (name)
            (if (file-directory-p name)
                (concat name "/")
              name))
          names))


(defun nav-find-files (pattern)
  "Finds files whose names match a regular expression, in '.' and all subdirs."
  (interactive "sFilename regex: ")
  (let* ((filenames (nav-get-non-boring-filenames-recursively "."))
         (names-matching-pattern
          (remove-if-not (lambda (name) (string-match pattern name)) filenames))
         (names-matching-pattern
          (nav-append-slashes-to-dir-names names-matching-pattern))
         (saved-directory default-directory))
    (pop-to-buffer nav-buffer-name-for-find-results nil)
    (setq default-directory saved-directory)
    (if names-matching-pattern
        (nav-show-find-results names-matching-pattern)
        (nav-replace-buffer-contents
         "No matching files found."
         nil))))


(defun nav-show-find-results (paths)
  "Displays the results when the user hits the 'f' key."
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((lines (mapcar (lambda (name)
			 (concat name ":1:"))
		       paths)))
    (insert (nav-join "\n" lines)))
  (grep-mode))


(defun nav-make-new-file (name)
  "Creates a new file."
  (interactive "sMake file: ")
  (let ((new-file-dir (expand-file-name default-directory)))
    (other-window 1)
    (nav-push-dir new-file-dir)
    (find-file name)
    (write-file name)
    (other-window 1)
    (nav-refresh)
    (other-window 1)))


(defun nav-make-new-directory (name)
  "Creates a new directory."
  (interactive "sMake directory: ")
  (make-directory name)
  (nav-refresh))


(defun nav-shell ()
  "Starts up a shell on the current nav directory."
  (interactive)
  (shell nav-shell-buffer-name)
  ;; Tell the shell to cd to the working directory of nav.
  (process-send-string (get-buffer-process nav-shell-buffer-name)
                       (format "cd '%s'\n" (nav-get-working-dir)))
  ;; Make sure the shell knows to do completion in the new directory.
  (shell-process-cd (nav-get-working-dir)))


(defun nav-term ()
  "Starts up a term on the current nav directory.

If there is already a *terminal* buffer then it is reused."
  (interactive)
  (let ((nav-temp-file "*nav-temp*"))
    (find-file-other-window nav-temp-file)
    (setq default-directory (nav-get-working-dir))
    (term "/bin/bash")
    (kill-buffer nav-temp-file)))


(defun nav-get-other-windows ()
  "Returns a list of windows other than the Nav window."
  (let* ((nav-window (get-buffer-window nav-buffer-name))
         (cur-window (next-window nav-window))
         (result '()))
    (while (not (eq cur-window nav-window))
      (if (not (window-minibuffer-p cur-window))
          (push cur-window result))
      (setq cur-window (next-window cur-window)))
    (reverse result)))


(defun nav-rotate-windows-cw ()
  "Cyclically permutes the windows other than the nav window, clockwise."
  (interactive)
  (nav-rotate-windows (lambda (i n) (mod (+ i 1) n))))


(defun nav-rotate-windows-ccw ()
  "Cyclically permutes the windows other than the nav window, counter-clockwise."
  (interactive)
  (nav-rotate-windows (lambda (i n) (mod (+ i n -1) n))))


(defun nav-rotate-windows (next-i)
  "Cyclically permutes the windows other than the nav window.

The permutation is either clockwise or counter-clockwise
depending on the passed-in function next-i."
  (let* ((win-list (nav-get-other-windows))
         (win-vec (apply 'vector win-list))
         (buf-list (mapcar 'window-buffer win-list))
         (buf-vec (apply 'vector buf-list))
         (n (length win-vec)))
    (dotimes (i n)
      (set-window-buffer (aref win-vec (funcall next-i i n))
                         (buffer-name (aref buf-vec i))))))


(defun nav-get-paths (dir-path)
  "Recursively finds all paths starting with a given directory name."
  (let* ((dir-path (file-name-as-directory dir-path))
         (paths (list dir-path)))
    (dolist (file-name (directory-files dir-path))
      (when (not (or (string= "." file-name)
                     (string= ".." file-name)))
            (let ((file-path (format "%s%s" dir-path file-name)))
              (if (and (file-directory-p file-path)
                       (not (file-symlink-p file-path)))
                  (let ((more-paths (nav-get-paths (format "%s/" file-path))))
                    (setq paths (append (reverse more-paths) paths)))
                (push file-path paths)))))
    (reverse paths)))


(defun nav-shell-command (command)
  "Runs a shell command and then refreshes the Nav window."
  (interactive "sShell command: ")
  (shell-command command)
  (nav-refresh))


(defun nav-resize-frame ()
  "Widens the frame to fit Nav without shrinking the editing space."
  (set-frame-width (selected-frame) 
                   (+ (frame-width) (nav-outer-width)))
  ;; set-frame-width resizes the nav window; set it back
  (nav-set-window-width nav-width))


(defun nav-print-current-dir ()
  "Shows the full path that nav is currently displaying"
  (interactive)
  (print default-directory))


(defun nav-screen-kill ()
  "Kills secondary nav screens."
  (interactive)
  (kill-buffer (buffer-name (current-buffer)))
  (other-window 1))


;; http://www.emacswiki.org/emacs/ElispCookbook#toc41
(defun nav-filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun nav-view-file ()
  "View file under cursor in read only mode. q to quit."
  (interactive)
  (nav-open-file-under-cursor)
  (let ((nav-view-file-map (make-sparse-keymap)))
    (use-local-map nav-view-file-map)
    (define-key nav-view-file-map "q" 'nav-screen-kill)
    (goto-line 1)
    (view-mode -1)
    (toggle-read-only 1)))


(defun nav-toggle-preview ()
  "Toggle nav preview mode."
  (interactive)
  (if (not nav-preview)
      (progn
        (nav-preview)
	(setq nav-preview t)
	(setq mode-line-format (nav-update-mode-line "d" default-directory)))
    (progn
      (nav-preview-stop)
      (setq nav-preview nil)))
  (nav-refresh))


(defun nav-help-screen ()
  "Displays the help screen outside the Nav window."
  (interactive)
  (other-window 1)
  (get-buffer-create "nav-help")
  (switch-to-buffer "nav-help")
  (let ((map (make-sparse-keymap)))
    (use-local-map map)
    (define-key map [mouse-1] 'nav-screen-kill)
    (define-key map [mouse-3] 'nav-screen-kill) 
    (define-key map [mouse-2] 'nav-screen-kill) 
    (define-key map "q" 'nav-screen-kill))
  (setq display-hourglass nil
        buffer-undo-list t)
  (insert "\
Help for nav directory listing mode
===================================

The numbers at the bottom are shortcuts.  5 opens the first 
bookmarked file (or 'quickfile') and so on. 8 takes you to the
first bookmarked directory (or 'quickdir') and so on. These 
directories and files can be changed by pressing the 'C' key 
and using the Emacs customization page that appears.

Key Bindings
============

Enter/Return: Open file or directory under cursor.
Tab: To move forward through buttons.
Shift-Tab: To move backward through buttons.

Space: Press then space then any other letter to jump to
       filename that starts with that letter.

1\t Open file under cursor in 1st other window.
2\t Open file under cursor in 2nd other window.

5\t Jump to 1st quick file.
6\t Jump to 2nd quick file.
7\t Jump to 3rd quick file.

8\t Jump to 1st quick dir.
9\t Jump to 2nd quick dir.
0\t Jump to 3rd quick dir.

a\t Make a new file.
b\t Toggle file/buffer browser (or Left-Mouse).
c\t Copy file or directory under cursor.
C\t Customize Nav settings and bookmarks.
d\t Delete file or directory under cursor (asks to confirm first).
e\t Edit current directory in dired.
f\t Recursively find files whose names or contents match some regexp.
F\t Toggles follow mode.
g\t Recursively grep for some regexp.
h\t Jump to home (~).
j\t Jump to another directory.
m\t Move or rename file or directory.
n\t Make a new directory.
o\t Switch to other window.
p\t Pop directory stack to go back to the directory where you just were.
P\t Print full path of current displayed directory.
q\t Quit nav.
r\t Refresh.
s\t Start a shell in an emacs window in the current directory.
t\t Expand tags on selected file (or Shift-Left-Mouse).
u\t Go up to parent directory.
v\t View file in read-only mode. Press q to close file.
V\t Toggle preview mode.
w\t Shrink-wrap Nav's window to fit the longest filename in the current directory.
W\t Set the window width to its default value.
!\t Run shell command.
[\t Rotate non-nav windows counter clockwise.
]\t Rotate non-nav windows clockwise.
.\t Toggle hidden files.
?\t Show this help screen.

                Press 'q' or click mouse to quit help

")
  (goto-line 1)
  (view-mode -1)
  (toggle-read-only 1))


(define-derived-mode nav-mode fundamental-mode 
  "Nav-mode is for IDE-like navigation of directories.

Nav is more IDEish than dired, and lighter weight than speedbar."
  (nav-set-window-width nav-width)
  (setq mode-name "Navigation")
  (use-local-map nav-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (if nav-hidden
      (setq nav-filter-regexps nav-no-hidden-boring-file-regexps))
  (if nav-follow
      (nav-start-timer))
  (nav-refresh))


(defun nav-disable-annoying-emacs23-window-splitting ()
  "Effectively turns off the unfortunate new feature where Emacs 23
automatically splits windows when opening files in a large frame."
  (setq split-width-threshold most-positive-fixnum)
  (setq split-height-threshold most-positive-fixnum))


;; The next line is for ELPA, the Emacs Lisp Package Archive.
;;;###autoload
(defun nav ()
  "Runs nav-mode in a narrow window on the left side, or quits Nav
if it's already running."
  (interactive)
  (if (nav-is-open)
      (nav-quit)
    ;;(nav-disable-annoying-emacs23-window-splitting)
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (ignore-errors (kill-buffer nav-buffer-name))
    (pop-to-buffer nav-buffer-name nil)
    (set-window-dedicated-p (selected-window) t)
    (nav-mode)
    (when nav-resize-frame-p
      (nav-resize-frame))))


(defun nav-follow-buffer ()
  "Tells Nav to display the contents of the current directory."
  (interactive)
  (if (not (string= nav-buffer-name (buffer-name (current-buffer))))
      (progn
	(let ((dir default-directory)
	      (win (buffer-name (current-buffer))))
	  (select-window (nav-get-window nav-buffer-name))
	  (nav-push-dir dir)
	  (select-window (nav-get-window win))))))


(defun nav-select-nav-window ()
  "Makes the Nav window active. Nav must already be running or this will fail."
  (select-window (nav-get-window nav-buffer-name)))


(defun nav-ensure-that-nav-is-running ()
  "Runs Nav if it is not already running."
  (if (not (nav-is-open))
      (nav)))


(define-key menu-bar-showhide-menu [Nav]
  '(menu-item "Nav" nav
	      :help "Turn Nav on/off"))


(provide 'nav)

;;; nav.el ends here
