;;; ensime-auto-complete.el
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

(require 'auto-complete)

(defun ensime-ac-delete-text-back-to-call-target ()
  "Assuming the point is in a member prefix, delete all text back to the
target of the call. Point should be be over last character of call target."
  (let ((p (point)))
    (re-search-backward "[^\\. ][\\. ]" (point-at-bol) t)
    (let ((text (buffer-substring-no-properties (1+ (point)) p))
	  (deactivate-mark nil))
      (delete-region (1+ (point)) p)
      text)))

(defun ensime-ac-member-candidates (prefix)
  "Return candidate list."
  (let ((members
	 (ensime-ac-with-buffer-copy

	  ;; Make some space so trailing characters don't interfere.
	  (save-excursion (insert " "))

	  ;; Delete the member prefix
	  (ensime-ac-delete-text-back-to-call-target)

	  ;; Add a trailing '.' so singleton object accesses parse correctly
	  ;; Move cursor forward so it will be on '.'
	  (forward-char)
	  (save-excursion
	    (insert ". ()"))

	  (ensime-write-buffer)
	  (ensime-rpc-members-for-type-at-point prefix))))

    (mapcar (lambda (m)
	      (let* ((type-sig (plist-get m :type-sig))
		     (type-id (plist-get m :type-id))
		     (is-callable (plist-get m :is-callable))
		     (name (plist-get m :name))
		     (candidate name))
		;; Save the type for later display
		(propertize candidate
			    'symbol-name name
			    'type-sig type-sig
			    'type-id type-id
			    'is-callable is-callable
			    'summary (ensime-ac-trunc-summary type-sig)
			    )))
	    members)
    ))


(defmacro* ensime-ac-with-buffer-copy (&rest body)
  "Create a duplicate of the current buffer, copying all contents.
Bind ensime-buffer-connection and buffer-file-name to the given values.
Execute forms in body in the context of this new buffer. The idea is that
We can abuse this buffer, even saving its contents to disk, and all the
changes will be forgotten."
  `(let ((buf (current-buffer))
	 (file-name buffer-file-name)
	 (p (point))
	 (conn (ensime-current-connection)))

     (unwind-protect
	 (with-temp-buffer
	   (let ((ensime-buffer-connection conn)
		 (buffer-file-name file-name))
	     (insert-buffer-substring buf)
	     (goto-char p)
	     ,@body
	     ))
       ;; Make sure we overwrite any changes
       ;; written from temp buffer.
       (ensime-write-buffer nil t)
       )))


(defun ensime-ac-completing-constructor-p (prefix)
  "Are we trying to complete a call of the form 'new [prefix]' ?"
  (save-excursion
    (goto-char (- (point) (length prefix)))
    (looking-back "new\\s-+" (ensime-pt-at-end-of-prev-line))
    ))

(defun ensime-ac-name-candidates (prefix)
  "Return candidate list."
  (let ((is-constructor (ensime-ac-completing-constructor-p prefix)))
    (let ((names
	   (ensime-ac-with-buffer-copy
	    (backward-delete-char (length prefix))
	    (insert ";{")
	    (save-excursion
	      ;; Insert a dummy value after (point), so that
	      ;; if we are at the end of a method body, the
	      ;; method context will be extended to include
	      ;; the completion point.
	      (insert "  ;exit()};"))
	    (ensime-write-buffer)
	    (ensime-rpc-name-completions-at-point
	     prefix is-constructor))))

      (mapcar (lambda (m)
		(let* ((type-sig (plist-get m :type-sig))
		       (type-id (plist-get m :type-id))
		       (is-callable (plist-get m :is-callable))
		       (name (plist-get m :name))
		       (candidate name))
		  ;; Save the type for later display
		  (propertize candidate
			      'symbol-name name
			      'type-sig type-sig
			      'type-id type-id
			      'is-callable is-callable
			      'summary (ensime-ac-trunc-summary type-sig)
			      ))
		) names))))


(defun ensime-ac-package-decl-candidates (prefix)
  "Return candidate list."
  (when (looking-back ensime-ac-package-decl-prefix-re
		      (ensime-pt-at-end-of-prev-line))
    (let* ((full-match (match-string 0))
	   (path (ensime-kill-txt-props (match-string 1)))

	   (names (ensime-ac-with-buffer-copy
		   (backward-delete-char (length full-match))
		   (insert "object ensimesynthetic${")

		   (if (eq (length path) 0)

		       (progn
			 (save-excursion
			   (insert "  }"))
			 (ensime-write-buffer)
			 (ensime-rpc-name-completions-at-point prefix))

		     (progn
		       (insert path)
		       (save-excursion
			 (insert "  }"))
		       (backward-char 2)
		       (ensime-write-buffer)
		       (ensime-rpc-members-for-type-at-point prefix))))))

      (delete-dups
       (mapcar (lambda (m) (plist-get m :name))
	       names)))))


(defun ensime-ac-trunc-summary (str)
  (let ((len (length str)))
    (if (> len 40)
	(concat (substring str 0 40) "...")
      str)))

(defun ensime-ac-candidate-name (c)
  (get-text-property 0 'symbol-name c))

(defun ensime-ac-candidate-type-sig (c)
  (get-text-property 0 'type-sig c))

(defun ensime-ac-get-doc (item)
  "Return doc for given item."
  (get-text-property 0 'type-sig item))

(defun ensime-pt-at-end-of-prev-line ()
  (save-excursion (forward-line -1)
		  (min
		   (- (point) 1)
		   (point-at-eol))))

(defun ensime-ac-member-prefix ()
  "Starting at current point. Find the point of completion for a member access.
   Return nil if we are not currently looking at a member access."
  (let ((point (re-search-backward "[\\. ]+\\([^\\. ]*\\)?" (point-at-bol) t)))
    (if point (1+ point))))


(defvar ensime-ac-name-following-keyword-re
  (concat
   "\\(?:\\W\\|\\s-\\)\\(?:else\\|case\\|new\\|with\\|extends\\|yield\\)"
   "\\s-+\\(\\w*\\)"))

(defvar ensime-ac-name-following-syntax-re
  (concat
   "[!:=>(\\[,;}{\n+*/\\^&~%-]"
   "\\s-*\\(\\w*\\)"))

(defun ensime-ac-name-prefix ()
  "Starting at current point - find the point of completion for a symbol.
 Return nil if we're looking at a context where symbol completion is
 inappropriate."
  (let ((left-bound (ensime-pt-at-end-of-prev-line)))
    (when (or (looking-back  ensime-ac-name-following-keyword-re left-bound)
	      (looking-back ensime-ac-name-following-syntax-re left-bound))
      (let ((point (- (point) (length (match-string 1)))))
	(goto-char point)
	point
	))))

(defvar ensime-ac-package-decl-prefix-re
  "\\(?:package\\|import\\)[ ]+\\(\\(?:[a-z0-9]+\\.\\)*\\)\\([A-z0-9]*\\)")
(defun ensime-ac-package-decl-prefix ()
  "Starting at current point. Find the point of completion for a member access.
   Return nil if we are not currently looking at a member access."
  (let ((left-bound (ensime-pt-at-end-of-prev-line)))
    (when (looking-back ensime-ac-package-decl-prefix-re left-bound)
      (let ((point (- (point) (length (match-string 2)))))
	(goto-char point)
	point))))


(defun ensime-ac-complete-action ()
  "Defines action to perform when user selects a completion candidate.

Delete the candidate from the buffer as inserted by auto-complete.el
 (because the candidates include type information that we don't want
 inserted), and re-insert just the name of the candidate.

If the candidate is a callable symbol, add the meta-info about the
params and param types as text-properties of the completed name. This info will
be used later to give contextual help when entering arguments."

  (let* ((candidate candidate) ;;Grab from dynamic environment..
	 (name candidate)
	 (type-id (get-text-property 0 'type-id candidate)))

    (let ((name-start-point (- (point) (length name))))

      ;; If this member is callable, use the type-id to lookup call completion
      ;; information to show parameter hints.
      (when (get-text-property 0 'is-callable candidate)

	(let* ((call-info (ensime-rpc-get-call-completion type-id))
	       (param-sections (ensime-type-param-sections call-info)))
	  (when (and call-info param-sections)

	    ;; Insert space or parens depending on the nature of the
	    ;; call
	    (save-excursion
	      (if (and (= 1 (length (car param-sections)))
		       (null (string-match "[A-z]" name)))
		  ;; Probably an operator..
		  (insert " ")
		;; Probably a normal method call
		(insert "()" )))

	    (if (car param-sections)
		(progn
		  ;; Save param info as a text properties of the member name..
		  (add-text-properties name-start-point
				       (+ name-start-point (length name))
				       (list 'call-info call-info
					     ))

		  ;; Setup hook function to show param help later..
		  (add-hook 'post-command-hook
			    'ensime-ac-update-param-help nil t)
		  ;; This command should trigger help hook..
		  (forward-char))

	      ;; Otherwise, skip to the end
	      (forward-char 2))

	    ))))))


(defun ensime-ac-get-active-param-info ()
  "Search backward from point for the param info of the call that
   we are currently completing."
  (save-excursion
    (catch 'return
      (let ((lbound (point-at-bol)) ;; TODO <-- what about multiline param lists
	    (balance 0))
	(backward-char 1)
	(while (> (point) lbound)
	  (cond
	   ((ensime-in-string-or-comment (point)) nil)
	   ((looking-at "\\s)") (decf balance))
	   ((looking-at "\\s(") (incf balance))
	   (t
	    (let ((call-info (get-text-property (point) 'call-info)))
	      (if (and (or (> balance 0)) call-info)
		  (throw 'return (list
				  :name-end-point (point)
				  :call-info call-info))))))
	  (backward-char 1))))))


(defun ensime-ac-update-param-help ()
  "When entering the arguments to a call, display a tooltip
   with the param names and types of the call."
  (let ((info (ensime-ac-get-active-param-info)))
    (if info
	(let* (;; To be used for tooltip positioning..
	       (name-end (plist-get info :name-end-point))
	       (call-info (plist-get info :call-info))
	       (signature (ensime-ac-call-info-signature call-info)))
	  (message signature))
      (remove-hook 'post-command-hook 'ensime-ac-update-param-help t))))


(defun ensime-ac-call-info-signature (call-info)
  "Return a pretty string representation of a call-info object."
  (let ((param-sections (plist-get call-info :param-sections))
	(result-type (plist-get call-info :result-type)))
    (concat
     (mapconcat
      (lambda (sect)
	(let ((params (plist-get sect :params))
	      (is-implicit (plist-get sect :is-implicit)))
	  (propertize (concat "("
			      (mapconcat
			       (lambda (nm-and-tp)
				 (format
				  "%s:%s"
				  (propertize (car nm-and-tp)
					      'face font-lock-variable-name-face)
				  (propertize (ensime-type-name-with-args
					       (cadr nm-and-tp))
					      'face font-lock-type-face)
				  ))
			       params ", ") ")")
		      'face (when is-implicit font-lock-comment-face)
		      )))
      param-sections " => ")
     " => "
     (propertize
      (ensime-type-name-with-args result-type)
      'face font-lock-type-face)
     )))


(ac-define-source ensime-members
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-member-candidates ac-prefix))
    (prefix . ensime-ac-member-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "f")
    (cache . t)
    ))

(ac-define-source ensime-scope-names
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-name-candidates ac-prefix))
    (prefix . ensime-ac-name-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "s")
    (cache . t)
    ))

(ac-define-source ensime-package-decl-members
  '((candidates . (ensime-ac-package-decl-candidates ac-prefix))
    (prefix . ensime-ac-package-decl-prefix)
    (requires . 0)
    (symbol . "s")
    (cache . t)
    ))

(defun ensime-ac-enable ()
  (make-local-variable 'ac-sources)

  ;; Note, we try to complete names before members.
  ;; This simplifies the regexes.
  (setq ac-sources '(ac-source-ensime-package-decl-members
		     ac-source-ensime-scope-names
		     ac-source-ensime-members ))

  (make-local-variable 'ac-use-comphist)
  (setq ac-use-comphist t)

  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start nil)

  (make-local-variable 'ac-expand-on-auto-complete)
  (setq ac-expand-on-auto-complete nil)

  (make-local-variable 'ac-use-fuzzy)
  (setq ac-use-fuzzy nil)

  (make-local-variable 'ac-use-quick-help)
  (setq ac-use-quick-help t)

  (make-local-variable 'ac-delete-dups)
  (setq ac-delete-dups nil)

  (make-local-variable 'ac-ignore-case)
  (setq ac-ignore-case t)

  (make-local-variable 'ac-trigger-key)
  (ac-set-trigger-key "TAB")

  (auto-complete-mode 1)
  )

(defun ensime-ac-disable ()
  (auto-complete-mode 0)
  )

(provide 'ensime-auto-complete)