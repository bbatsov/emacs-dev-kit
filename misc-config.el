;;; miscellaneous configuration that doesn't
;;; belong to any particular place
;;; This file is part of the Emacs Dev Kit

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)                     ; show line number
(column-number-mode t)                   ; show column number
(size-indication-mode t)                 ; show file size (Emacs 22+)

;; general settings
(setq-default indent-tabs-mode nil)      ; I hate tabs!

(delete-selection-mode t)                ; delete the selection with a keypress

(unless (= emacs-major-version 24) 
  (setq x-select-enable-clipboard t       ; copy-paste should work ...
        interprogram-paste-function       ; ...with...
        'x-cut-buffer-or-selection-value)) ; ...other X clients

(setq search-highlight t                 ; highlight when searching...
      query-replace-highlight t)         ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ; enable y/n answers to yes/no

(global-font-lock-mode t)                ; always do syntax highlighting
(setq require-final-newline t)           ; end files with a newline

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; utf8 / input-method
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal

;; saveplace: save location in file when saving files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package

;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
      '(search ring regexp-search-ring)    ;; ... my search entries
      savehist-autosave-interval 60        ;; save every minute (default: 5 min)
      savehist-file (concat "~/.emacs.d" "/savehist"))   ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation

;; save recent files
(setq recentf-save-file (concat dotfiles-dir "recentf") ;; keep ~/ clean
      recentf-max-saved-items 100          ;; max save 100
      recentf-max-menu-items 15)         ;; max 15 in menu
(recentf-mode t)                  ;; turn it on

;; time-stamps
;; when there's "Time-stamp: <>" in the first 10 lines of the file
(setq
 time-stamp-active t        ; do enable time-stamps
 time-stamp-line-limit 10   ; check first 10 buffer lines for Time-stamp: <>
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)

;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

;; now we can use tramp to open files
;; requiring root access
(defun find-alternative-file-with-sudo ()
  "Open current buffer as root!"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     sudo/su can be used as well, but they
     do not work for me
     (concat "/ssh:root@localhost:"
             buffer-file-name))))

;; ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)

;; auto-completion in minibuffer
(icomplete-mode +1)

(set-default 'imenu-auto-rescan t)

;; flyspell-mode
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive argument.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

;; Default to unified diffs
(setq diff-switches "-u")

;; yasnippet - http://code.google.com/p/yasnippet/
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat ext-dir "yasnippet-0.6.1c/snippets"))

(setq auto-mode-alist
      (cons '("\\.yasnippet" . snippet-mode) auto-mode-alist))

;; magit
(require 'magit)

;; subversion
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;; dired-mode - enable the reuse of the same dired buffer with 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; enable narrow to region
(put 'narrow-to-region 'disabled nil)

;; bookmarks
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1)                        ;; autosave each change

;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(eval-after-load 'paredit-mode
  ;; need a binding that works over SSH
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

;; whitespace-mode
;; free of trailing whitespace and to use 80-column width, standard indentation
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80)

;; automatic buffer clean-up
(require 'midnight)
;; don't kill my precious manuals while I'm still reading them
(add-to-list 'clean-buffer-list-kill-never-buffer-names "*info*")

;; use Google Chrome as default browser
(setq  browse-url-browser-function 'browse-url-generic
       browse-url-generic-program "/usr/bin/google-chrome")

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; we want to be able to have images in tree widgets(especially in Garak)
(setq tree-widget-image-enable t)

;; gist integration
(require 'gist)

;; zenburn color theme setup
(if (>= emacs-major-version 24)
    (load-theme 'zenburn t)
  (progn
    (require 'color-theme-zenburn)
    (color-theme-zenburn)))


;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(global-hl-line-mode t) ; turn it on for all modes by default

;; simpler way to navigate the contents of the kill-ring
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

(if (>= emacs-major-version 24)
    (progn
      (electric-pair-mode t)
      (electric-indent-mode t)
      (electric-layout-mode t))
  (progn
    ;; autopair mode
    (require 'autopair)
    (autopair-global-mode)
    (setq autopair-autowrap t)
    (setq autopair-blink nil)

    (add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))))

;; add double quotes to text-mode syntax table
(add-hook 'text-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?\" "\"")))

(add-hook 'erc-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?\" "\"")))

;; turn on auto-fill in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; emacs nav - a simple file browser for Emacs
(require 'nav)

(setq nav-follow t)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown$" . markdown-mode)
            (cons '("\\.md$" . markdown-mode) auto-mode-alist)))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; graphviz-mode
(load-file (concat ext-dir "graphviz-dot-mode.el")) 


(autoload 'find-file-in-project "find-file-in-project"
  "Find file in project." t)

;; indent pasted code
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; abbrev config
(add-hook 'text-mode-hook (lambda () (abbrev-mode +1)))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(require 're-builder)
(setq reb-re-syntax 'string)

;; projectile is a project management mode
(require 'projectile)
(projectile-global-mode t)

(provide 'misc-config)
