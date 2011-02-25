;;; ensime-refactor.el
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


(defvar ensime-refactor-id-counter 0
  "Each refactoring is given a unique id.")

(defvar ensime-refactor-info-buffer-name "*ENSIME-Refactoring*")

(defvar ensime-refactor-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (lambda()(interactive)
				(funcall continue-refactor)
				(ensime-popup-buffer-quit-function)
				))
    (define-key map (kbd "q") (lambda()(interactive)
				(funcall cancel-refactor)
				(ensime-popup-buffer-quit-function)
				))
    map)
  "Key bindings for the refactor confirmation popup.")


(defun ensime-refactor-notify-failure (result)
  (message "Refactoring failed: %s" (plist-get result :reason)))


(defun ensime-refactor-sym-at-point ()
  "Return information about the symbol at point. If not looking at a
 symbol, return nil."
  (let ((start nil)
	(end nil))

    (when (thing-at-point 'symbol)

      (save-excursion
	(search-backward-regexp "\\W" nil t)
	(setq start (+ (point) 1)))
      (save-excursion
	(search-forward-regexp "\\W" nil t)
	(setq end (- (point) 1)))
      (list :start start
	    :end end
	    :name (buffer-substring-no-properties start end)))))




(defun ensime-refactor-organize-imports ()
  "Do a syntactic organization of the imports in the current buffer."
  (interactive)
  (ensime-refactor-perform
   'organizeImports
   `(file ,buffer-file-name)))


(defun ensime-refactor-rename (&optional new-name)
  "Rename a symbol, project-wide."
  (interactive)
  (let ((sym (ensime-refactor-sym-at-point)))
    (if sym
	(let* ((start (plist-get sym :start))
	       (end (plist-get sym :end))
	       (old-name (plist-get sym :name))
	       (name (or new-name
			 (read-string (format "Rename '%s' to: " old-name)))))
	  (ensime-refactor-perform
	   'rename
	   `(file ,buffer-file-name
		  start ,(- start ensime-ch-fix)
		  end ,(- end ensime-ch-fix)
		  newName ,name)))
      (message "Please place cursor on a symbol."))))


(defun ensime-refactor-inline-local ()
  "Get rid of an intermediate variable."
  (interactive)
  (let ((sym (ensime-refactor-sym-at-point)))
    (if sym
	(let* ((start (plist-get sym :start))
	       (end (plist-get sym :end)))
	  (ensime-refactor-perform
	   'inlineLocal
	   `(file ,buffer-file-name
		  start ,(- start ensime-ch-fix)
		  end ,(- end ensime-ch-fix))))
      (message "Please place cursor on a local value."))))


(defun ensime-refactor-extract-method ()
  "Extract a range of code into a method."
  (interactive)
  (let* ((name (read-string "Name of method: ")))
    (ensime-refactor-perform
     'extractMethod
     `(file ,buffer-file-name
	    start ,(- (mark) ensime-ch-fix)
	    end ,(- (point) ensime-ch-fix)
	    methodName ,name))))


(defun ensime-refactor-extract-local ()
  "Extract a range of code into a val."
  (interactive)
  (let* ((name (read-string "Name of local value: ")))
    (ensime-refactor-perform
     'extractLocal
     `(file ,buffer-file-name
	    start ,(- (mark) ensime-ch-fix)
	    end ,(- (point) ensime-ch-fix)
	    name ,name))))


(defun ensime-refactor-perform (refactor-type params)
  (ensime-assert-buffer-saved-interactive
   (incf ensime-refactor-id-counter)
   (message "Please wait...")
   (ensime-rpc-refactor-perform
    ensime-refactor-id-counter
    refactor-type
    params
    'ensime-refactor-perform-handler
    )))

(defun ensime-refactor-perform-handler (result)
  (let ((refactor-type (plist-get result :refactor-type))
	(status (plist-get result :status))
	(id (plist-get result :procedure-id))
	(changes (plist-get result :changes)))
    (if (equal status 'success)
	(let ((cont `(lambda () (ensime-rpc-refactor-exec
				 ,id ',refactor-type
				 'ensime-refactor-handle-result)))
	      (cancel `(lambda () (ensime-rpc-refactor-cancel ,id))))

	  (ensime-with-popup-buffer
	   (ensime-refactor-info-buffer-name t t)
	   (use-local-map ensime-refactor-info-map)
	   (set (make-local-variable 'cancel-refactor) cancel)
	   (set (make-local-variable 'continue-refactor) cont)
	   (ensime-refactor-populate-confirmation-buffer
	    refactor-type changes)
	   (goto-char (point-min)))

	  (ensime-event-sig :refactor-at-confirm-buffer))

      (ensime-refactor-notify-failure result)
      )))


(defun ensime-refactor-handle-result (result)
  (let ((touched (plist-get result :touched-files)))
    (ensime-revert-visited-files touched t)
    (ensime-event-sig :refactor-done touched)
    ))

(defun ensime-refactor-populate-confirmation-buffer (refactor-type changes)
  (let ((header
	 "Please review the proposed changes."))

    (ensime-insert-with-face
     (concat header " (c to confirm, q to cancel)")
     'font-lock-constant-face)
    (insert "\n\n\n")

    (if (null changes)
	(insert "Nothing to be done.")
      (ensime-insert-change-list changes))))



(provide 'ensime-refactor)