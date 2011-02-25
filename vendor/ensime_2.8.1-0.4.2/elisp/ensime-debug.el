;;; ensime-debug.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(defgroup ensime-db nil
  "Customization of ensime debugger support."
  :group 'ensime
  :prefix 'ensime-db)

(defcustom ensime-db-cmd-template
  '("jdb" "-classpath" :classpath
    "-sourcepath" :sourcepath :debug-class :debug-args)
  "The command to launch the debugger. Keywords will be replaced
 with data loaded from server."
  :type 'string
  :group 'ensime-db)

(defcustom ensime-db-buffer-name "*ENSIME-db*"
  "Buffer name for debug process"
  :type 'string :group 'ensime-db)

(defcustom ensime-db-default-cmd-line '("jdb")
  "Default command to launch the debugger, used when not connected to an ENSIME
server."
  :type 'string
  :group 'ensime-db)

(defface ensime-breakpoint-face
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen"))
    (t (:bold t)))
  "Face used for marking lines with breakpoints."
  :group 'ensime-ui)

(defface ensime-marker-face
  '((((class color) (background dark)) (:background "DarkGoldenrod4"))
    (((class color) (background light)) (:background "DarkGoldenrod2"))
    (t (:bold t)))
  "Face used for marking lines with breakpoints."
  :group 'ensime-ui)

(defvar ensime-db-default-main-args nil
  "History of arguments passed to main class.")

(defvar ensime-db-default-main-class nil
  "History of main class to debugger.")


(defvar ensime-db-history nil
  "History of argument lists passed to jdb.")

(defvar ensime-db-output-acc "")

(defvar ensime-db-output-acc-max-length 50000)

(defvar ensime-db-filter-funcs
  `(("Deferring breakpoint \\(.+\\):\\([0-9]+\\)\n[^ ]" .
     ensime-db-handle-deferred-breakpoint)

    ("Set breakpoint \\(.+\\):\\([0-9]+\\)\n[^ ]" .
     ensime-db-handle-set-breakpoint)

    ("Removed: breakpoint \\(.+\\):\\([0-9]+\\)\n[^ ]" .
     ensime-db-handle-removed-breakpoint)

    ("Not found: breakpoint \\(.+\\):\\([0-9]+\\)\n[^ ]" .
     ensime-db-handle-not-found-breakpoint)

    ("Breakpoints set:\\(?:[ \t\n]+breakpoint \\(.+\\):\\([0-9]+\\)\\)*\n[^ ]" . 
     ensime-db-handle-breakpoints-list)

    ("No breakpoints set.\n[^ ]" .
     ensime-db-handle-empty-breakpoints-list)

    ("Breakpoint hit: \"[^\"]+\", \\([^,]+\\), line=\\([0-9]+\\) bci=[0-9]+\n[^ ]" .
     ensime-db-handle-breakpoint-hit)

    ("Step completed: \"[^\"]+\", \\([^,]+\\), line=\\([0-9]+\\) bci=[0-9]+\n[^ ]" .
     ensime-db-handle-step)

    ("Nothing suspended.\n[^ ]" .
     ensime-db-handle-nothing-suspended)

    ("No local variables\n[^ ]" .
     ensime-db-handle-no-local-variables)

    ("is not valid until the VM is started with the 'run' command\n[^ ]" .
     ensime-db-handle-not-valid-until-run)

    (,(concat "Method arguments:\\(?:[\n]+.+=.+\\)*\n"
	      "Local variables:\\(?:[\n]+.+=.+\\)*\n"
	      "[^=]+\n" ) .
	      ensime-db-handle-local-variables)
    ))

(defun ensime-db-handle-not-valid-until-run (str)
  (message "Command not valid until the VM is started with the 'run' command, C-c d r."))

(defun ensime-db-handle-no-local-variables (str)
  (message "No local variables."))

(defun ensime-db-handle-local-variables (str)
  (save-selected-window
    (switch-to-buffer-other-window "*ensime-db-locals*")
    (erase-buffer)
    (let ((lines (split-string (match-string 0 str) "\n")))
      (dolist (l lines)
	(cond
	 ((equal l "Method arguments:")
	  (ensime-insert-with-face
	   (concat "\n" l "\n")
	   font-lock-type-face))
	 ((equal l "Local variables:")
	  (ensime-insert-with-face
	   (concat "\n" l "\n")
	   font-lock-type-face))
	 (t (progn
	      (when (string-match "\\(.+\\) = \\(.+\\)" l)
		(ensime-insert-with-face
		 (match-string 1 l) font-lock-variable-name-face)
		(insert " = ")
		(ensime-insert-with-face
		 (match-string 2 l) font-lock-constant-face)
		(insert "\n")
		)))
	 )))
    (setq buffer-read-only t)
    ))

(defun ensime-db-handle-nothing-suspended (str)
  (message "Nothing suspended."))

(defun ensime-db-handle-deferred-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "Deferred breakpoint: %s : %s" class line)))

(defun ensime-db-handle-set-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "Set breakpoint: %s : %s" class line)))

(defun ensime-db-handle-removed-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "Removed breakpoint: %s : %s" class line)))

(defun ensime-db-handle-breakpoint-hit (str)
  (let ((class-and-method (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (ensime-db-set-debug-marker class-and-method line)))

(defun ensime-db-handle-breakpoint-hit (str)
  (let* ((class-and-method (match-string 1 str))
	 (line (string-to-number (match-string 2 str)))
	 (class (ensime-db-class-from-class-and-method class-and-method)))
    (ensime-db-set-debug-marker class line)))

(defun ensime-db-handle-step (str)
  (let* ((class-and-method (match-string 1 str))
	 (line (string-to-number (match-string 2 str)))
	 (class (ensime-db-class-from-class-and-method class-and-method)))
    (ensime-db-set-debug-marker class line)))

(defun ensime-db-handle-not-found-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "No breakpoint to clear at: %s : %s" class line)))

(defun ensime-db-handle-empty-breakpoints-list (str)
  (ensime-db-clear-breakpoint-overlays))

(defun ensime-db-handle-breakpoints-list (str)
  "If we find a listing of breakpoint locations, use that information
to refresh the buffer overlays."
  (ensime-db-clear-breakpoint-overlays)
  (let* ((start (match-beginning 0))
	 (end (match-end 0))
	 (pos start)
	 (bp-list '()))

    ;; Build a list of class,line pairs
    (while
	(and (< pos end)
	     (integerp
	      (string-match "breakpoint \\(.+\\):\\([0-9]+\\)" str pos)))
      (setq pos (match-end 0))
      (let ((class (match-string 1 str))
	    (line (string-to-number (match-string 2 str))))
	(push (list class line) bp-list)))

    ;; Lookup source locations of class,line pairs.
    ;; Create buffer overlays...
    (let ((bp-locs (ensime-rpc-debug-class-locs-to-source-locs bp-list)))
      (dolist (bp bp-locs)
	(let ((file (car bp))
	      (line (cadr bp)))
	  (when (and (stringp file) (integerp line))
	    (when-let (ov (ensime-make-overlay-at
			   file line nil nil
			   "Breakpoint"
			   'ensime-breakpoint-face))
	      (push ov ensime-db-breakpoint-overlays))))))

    ))


(defun ensime-db-class-from-class-and-method (class-and-method)
  "Given qualified class+method of form package.class.method() return
just the qualified class, package.class."
  (if (integerp (string-match "\\(.+\\)\\..+()" class-and-method))
      (match-string 1 class-and-method)
    class-and-method))


(defun ensime-db-set-debug-marker (class line)
  "Find source location for given qualified class and line. Open
that location in a new window, *without* changing the active buffer."
  (let ((locs (ensime-rpc-debug-class-locs-to-source-locs
	       (list (list class line)))))
    (let* ((loc (car locs))
	   (file (car loc))
	   (line (cadr loc)))

      (when (and file (integerp line))

	;; comint filters must not change active buffer!
	(save-selected-window

	  (ensime-db-clear-marker-overlays)
	  (when-let (ov (ensime-make-overlay-at
			 file line nil nil
			 "Debug Marker"
			 'ensime-marker-face))
	    (push ov ensime-db-marker-overlays))

	  (ensime-goto-source-location
	   (list :file file :line line)
	   'window)

	  )))))


(defvar ensime-db-breakpoint-overlays '())

(defun ensime-db-clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay ensime-db-breakpoint-overlays)
  (setq ensime-db-breakpoint-overlays '()))

(defun ensime-db-refresh-breakpoint-overlays ()
  "Cause debugger to output a list of all active breakpoints.
Output filter will grab this output and use it to update overlays."
  (ensime-db-send-str "clear"))


(defvar ensime-db-marker-overlays '())

(defun ensime-db-clear-marker-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay ensime-db-marker-overlays)
  (setq ensime-db-marker-overlays '()))


(defun ensime-db-find-first-handler (str filters)
  "Find the car in filters that matches str earliest in the text.
Return that car's corresponding cdr (a filter function). Guarantees that
match-end, match-beginning, match-string are set correctly on return."
  (condition-case err
      (let ((best-start (length str))
	    (best-filter))

	(dolist (filter filters)
	  (let ((regexp (car filter)))
	    (when (and (integerp (string-match regexp str))
		       (< (match-beginning 0) best-start))
	      (setq best-start (match-beginning 0))
	      (setq best-filter filter))))

	;; Make sure match-end, match-beginning, match-string
	;; are correct on return
	(when best-filter
	  (string-match (car best-filter) str)
	  (cdr best-filter)))
    (error
     (message "Error in ENSIME debug output handler: %s" err))))


(defun ensime-db-output-filter (string)
  ;; Build up the accumulator.
  (setq ensime-db-output-acc (concat ensime-db-output-acc string))

  ;; We process string left to right.  Each time through the
  ;; following loop we select the best (earliest matching) filter
  ;; and call its handler. Then we delete ensime-db-output-acc up to
  ;; and including the match
  ;;
  ;; We make the assumption that the text sections matched by the
  ;; various filters do not overlap.
  (catch 'done
    (while
	(let ((handler
	       (ensime-db-find-first-handler
		ensime-db-output-acc ensime-db-filter-funcs)))
	  (if handler

	      ;; Save in case changed in handler
	      (let ((match-end-index (match-end 0)))

		(funcall handler ensime-db-output-acc)
		(setq ensime-db-output-acc
		      (substring ensime-db-output-acc match-end-index)))
	    (progn
	      (throw 'done t))))))

  ;; Do not allow accumulator to grow without bound.
  (when (> (length ensime-db-output-acc)
	   ensime-db-output-acc-max-length)
    (setq ensime-db-output-acc
	  (substring ensime-db-output-acc
		     (- (/ (* ensime-db-output-acc-max-length 3) 4)))))

  ;; We don't filter any debugger output so
  ;; just return what we were given.
  string)


(defun ensime-db-next ()
  "Cause debugger to go to next line, without stepping into
method invocations."
  (interactive)
  (ensime-db-send-str "next"))

(defun ensime-db-step ()
  "Cause debugger to go to next line, stepping into
method invocations."
  (interactive)
  (ensime-db-send-str "step"))

(defun ensime-db-continue ()
  "Continue stopped debugger."
  (interactive)
  (ensime-db-send-str "cont"))

(defun ensime-db-run ()
  "Start debugging the current program."
  (interactive)
  (ensime-db-send-str "run"))

(defun ensime-db-set-break (f line)
  "Set a breakpoint in the current source file at point."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (let* ((info (ensime-rpc-debug-unit-info (file-name-nondirectory f) line)))
    (if info
	(let ((class (plist-get info :full-name)))
	  (ensime-db-send-str (format "stop at %s:%s" class line)))
      (message "Could not find class information for given position.")))
  (ensime-db-refresh-breakpoint-overlays))


(defun ensime-db-clear-break (f line)
  "Set a breakpoint in the current source file at point."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (let* ((info (ensime-rpc-debug-unit-info (file-name-nondirectory f) line)))
    (if info
	(let ((class (plist-get info :full-name)))
	  (ensime-db-send-str (format "clear %s:%s" class line)))
      (message "Could not find class information for given position.")))
  (ensime-db-refresh-breakpoint-overlays))


(defun ensime-db-list-breakpoints (f line)
  "Cause debugger to output a list of all active breakpoints. Note, this will
cause the output filter to refresh the breakpoint overlays."
  (ensime-db-send-str "clear"))


(defun ensime-db-list-locals ()
  "Cause debugger to output a list of all local variables and
their values."
  (interactive)
  (ensime-db-send-str "locals"))

(defun ensime-db-quit ()
  "Stop debugging the current program. Kills the debug buffer."
  (interactive)
  (kill-buffer ensime-db-buffer-name))

(defun ensime-db-send-str (str &optional no-newline)
  "Sends a string to the debug process. Automatically append a newline."
  (interactive)
  (let* ((buf (get-buffer ensime-db-buffer-name))
	 (proc (get-buffer-process buf)))
    (if (not proc)
	(message "Project is not being debugged. Use M-x ensime-db-start to start debugger.")
      (comint-send-string proc (concat str (if no-newline "" "\n"))))))

(defun ensime-db-get-cmd-line ()
  "Get the command needed to launch a debugger, including all
the current project's dependencies. Returns list of form (cmd [arg]*)"
  (if (ensime-connected-p)
      (let* ((conf (ensime-rpc-debug-config))
	     (debug-class
	      (ensime-strip-dollar-signs
	       (ensime-completing-read-path
		"Qualified name of class to debug: "
		ensime-db-default-main-class)))
	     (debug-args (read-string
			  "Commandline arguments: "
			  ensime-db-default-main-args)))
	(setq ensime-db-default-main-class debug-class)
	(setq ensime-db-default-main-args debug-args)
	(plist-put conf :debug-class debug-class)
	(plist-put conf :debug-args
		   (ensime-tokenize-cmd-line debug-args))
	(ensime-flatten-list
	 (ensime-replace-keywords ensime-db-cmd-template conf)))
    ensime-db-default-cmd-line))

(defun ensime-db-start ()
  "Run a Scala interpreter in an Emacs buffer"
  (interactive)

  (ensime-with-conn-interactive
   conn
   (let ((root-path (or (ensime-configured-project-root) "."))
	 (cmd-line (ensime-db-get-cmd-line)))

     (save-selected-window
       (switch-to-buffer-other-window
	(get-buffer-create ensime-db-buffer-name))

       (comint-mode)

       (set (make-local-variable 'comint-prompt-regexp)
	    "^> \\|^[^ ]+\\[[0-9]+\\] ")
       (set (make-local-variable 'comint-process-echoes) nil)
       (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
       (set (make-local-variable 'comint-prompt-read-only) t)
       (set (make-local-variable 'comint-output-filter-functions)
	    '(ensime-db-output-filter comint-postoutput-scroll-to-bottom))

       (setq ensime-db-output-acc "")
       (setq ensime-buffer-connection conn)

       (add-hook 'kill-buffer-hook 'ensime-db-clear-breakpoint-overlays nil t)
       (add-hook 'kill-buffer-hook 'ensime-db-clear-marker-overlays nil t)
       (ensime-db-clear-breakpoint-overlays)
       (ensime-db-clear-marker-overlays)

       (cd root-path)
       (ensime-assert-executable-on-path (car cmd-line))
       (comint-exec (current-buffer)
		    "ensime-debug-cmd"
		    (car cmd-line)
		    nil (cdr cmd-line))

       (let ((proc (get-buffer-process (current-buffer))))
	 (ensime-set-query-on-exit-flag proc))))))



(provide 'ensime-debug)
