;;; nav-preview.el: Nav preview mode
;;
;; Author: matthew.ozor@gmail.com (Matthew Ozor)
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


(defvar nav-preview-pre-open-list nil
  "list of open buffers not to auto-close")

(defvar nav-preview-kill-list nil
  "list of buffers nav-preview opened")


(defun nav-preview-make-mode-map ()
  "Creates and returns a mode map with bufs's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n" 'nav-preview-next-line)
    (define-key keymap "p" 'nav-preview-previous-line)
    (define-key keymap "q" 'nav-preview-quit)
    (define-key keymap "V" 'nav-toggle-preview)
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "W" 'nav-set-width-to-default)
    (define-key keymap "?" 'nav-preview-help-screen)
    (define-key keymap " " 'scroll-other-window)
    (define-key keymap [(down)] 'nav-preview-down)
    (define-key keymap [(up)] 'nav-preview-up)
    (define-key keymap [(control ?n)] 'nav-preview-down)
    (define-key keymap [(control ?p)] 'nav-preview-up)
    keymap))

(setq nav-preview-mode-map (nav-preview-make-mode-map))


(defun nav-preview-down ()
 "Preview the next file down."
  (interactive)
  (next-line)
  (nav-preview-show))


(defun nav-preview-up ()
 "Preview the next file up."
  (interactive)
  (previous-line)
  (nav-preview-show))
  
  
(defun nav-preview-show ()
  (if (not (looking-at "^.*/$"))
      (progn
	(nav-open-file-under-cursor)
	(if (not (member (buffer-name (current-buffer)) 'nav-preview-pre-open-list))
	    (add-to-list 'nav-preview-kill-list (buffer-name (current-buffer))))
	(setq buffer-read-only t)
	(buffer-disable-undo)
	(select-window (nav-get-window nav-buffer-name)))))


(defun nav-preview-next-line ()
  "Move preview frame point down one line."
  (interactive)
  (other-window 1)
  (next-line)
  (select-window (nav-get-window nav-buffer-name)))
  

(defun nav-preview-previous-line ()
  "Move preview frame point up one line."
  (interactive)
  (other-window 1)
  (previous-line)
  (select-window (nav-get-window nav-buffer-name)))


(defun nav-preview-help-screen ()
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
Help for Nav Preview mode
========================

Key Bindings
============

Enter/Return: Jump to buffers under cursor.
Space: Srolls other window down one page.

n\t Move cursor in other window down line.
p\t Move cursor in other window up line.
q\t Quit Nav.
R\t Turn off Preview mode.
w\t Shrink-wrap Nav's window to fit the longest filename in the current directory.
W\t Set the window width to its default value.
?\t Show this help screen.


                Press 'q' or click mouse to quit help

")
  (goto-line 1)
  (view-mode -1)
  (toggle-read-only 1))


(define-derived-mode nav-preview-mode fundamental-mode 
  "Nav-buf-mode is displaying and switching buffers."
  (setq mode-name "Nav preview")
  (global-font-lock-mode nil)
  (use-local-map nav-preview-mode-map))


(defun nav-preview ()
  "Start nav preview mode."
  (interactive)
  (setq nav-preview-pre-open-list (mapcar (function buffer-name) (buffer-list)))
  (nav-save-cursor-line)
  (select-window (nav-get-window nav-buffer-name))
  (nav-preview-mode)
  (nav-preview-show))


(defun nav-preview-stop ()
  "Stops preview mode."
  (interactive)
  (dolist (b nav-preview-kill-list)
    (kill-buffer b))
  (setq nav-preview-kill-list nil)
  (setq nav-preview nil)
  (global-font-lock-mode 1)
  (nav-mode))


(defun nav-preview-quit ()
  "Quits Nav from within bufs mode."
  (interactive)
  (nav-preview-stop)
  (nav-quit))


(provide 'nav-preview)

;;; nav-preview.el ends here