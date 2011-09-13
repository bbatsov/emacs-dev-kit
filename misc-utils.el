;;; various helper functions
;;; This file is part of the Emacs Dev Kit

;; we need to do some clean up of a string before we send in to the shell
(defun clean-message (s)
  (setq s (replace-regexp-in-string "'" "&apos;"
  (replace-regexp-in-string "\"" "&quot;"
  (replace-regexp-in-string "&" "&amp;"
  (replace-regexp-in-string "<" "&lt;"
  (replace-regexp-in-string ">" "&gt;" s)))))))

(defun notify-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"
  (interactive)
  (when sound (shell-command
               (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "
                             (if icon (concat "-i " icon) "")
                             " '" title "' '" (clean-msg msg) "'"))
    ;; text only version
    (message (concat title ": " msg))))

;; simple function that allows us to open
;; the underlying file of a buffer in an external program
(defun open-with ()
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (read-shell-command "Open current file with: ")
                    " "
                    buffer-file-name))))

;; show some buffers
(defun show-some-buffers (buffer-list)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (dolist (buffer buffer-list)
    (split-window-vertically)
    (switch-to-buffer (get-buffer buffer))
    (other-window 1))
  ;; at the end we have one extra window we need to delete
  (delete-window)
  (balance-windows))

;; show some erc buffers
(defun show-erc-buffers ()
  (interactive)
  (show-some-buffers '("#emacs" "#clojure")))

(defun kill-buffers-by-mode (mode)
  (mapcar 'kill-buffer (filter-buffers-by-mode mode)))

(defun kill-erc-buffers ()
  (interactive)
  (kill-buffers-by-mode 'erc-mode))

(defun filter-buffers-by-mode (mode)
  (delq nil
        (mapcar
         (lambda (x) (and (eq (buffer-mode x) mode) x))
         (buffer-list))))

(defun buffer-mode (buffer-or-name)
  (with-current-buffer buffer-or-name major-mode))

(defun visit-term-buffer ()
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (ansi-term "/bin/zsh")
    (switch-to-buffer "*ansi-term*")))

;; toggle the input method and the flyspell dictionary
(defun toggle-bulgarian-input-method ()
  (interactive)
  (if (string= current-input-method "bulgarian-phonetic")
      (progn
        (inactivate-input-method)
        (ispell-change-dictionary "en_US"))
    (progn
      (set-input-method "bulgarian-phonetic")
      (ispell-change-dictionary "bulgarian"))))

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(defun indent-rigidly-and-copy-to-clipboard (begin end indent)
  "Copy the selected code region to the clipboard, indented according
to Markdown blockquote rules."
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) indent)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun indent-blockquote-and-copy-to-clipboard (begin end)
  "Copy the selected code region to the clipboard, indented according
to markdown blockquote rules (useful to copy snippets to StackOverflow, Assembla, Github."
  (interactive "r")
  (indent-rigidly-and-copy-to-clipboard begin end 4))

(defun indent-nested-blockquote-and-copy-to-clipboard (begin end)
  "Copy the selected code region to the clipboard, indented according
to markdown blockquote rules. Useful to add snippets under bullet points."
  (interactive "r")
  (indent-rigidly-and-copy-to-clipboard begin end 6))



(defun strip-presentation-effects ()
  "Strips effects from LaTeX Beamer presentations. Useful to create complementary pdf's with no effects."
  (interactive)
  (if (not (string= (substring (buffer-file-name) -4 nil) ".tex"))
      (error "You can run this only on LaTeX files"))
  (while (re-search-forward "\\\\pause" nil t)
    (replace-match "" nil nil))
  (while (re-search-forward "\\\\transdissolve" nil t)
    (replace-match "" nil nil)))

(provide 'misc-utils)
