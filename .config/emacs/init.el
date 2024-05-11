;;; init.el -*- lexical-binding: t -*-

(require 'multiple-cursors)
(require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
      (condition-case nil
          ((((((test)))))) (error nil))

(require 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)

(add-to-list 'custom-theme-load-path "~/.config/emacs.d/themes/")
(load-theme 'untitled t)

(menu-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode)
(prefer-coding-system 'utf-8)

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq version-control t)
(setq kept-new-versions 10)
(setq kept-old-versions 0)
(setq delete-old-versions t)
(setq auto-save-interval 50)

(let ((backup-dir (expand-file-name (concat user-emacs-directory "backups/")))
      (auto-save-dir (expand-file-name (concat user-emacs-directory "auto-saves/"))))
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir t))
  (if (not (file-exists-p auto-save-dir))
      (make-directory auto-save-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

; enable local variable check
(setq enable-local-eval 'maybe
      enable-local-variables t)

; yes or no -> y or n
(setq use-short-answers t)

; disable startup screen
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

; set startup echo area message
(defun display-startup-echo-area-message ()
  (let ((user-name (getenv "USER")))
    (message "hi %s, happy hacking!" user-name)))

; no deceased buffer jobs confirmation
(setq confirm-kill-processes nil)

; prefer spaces over tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

; maximum fill column
(setq-default fill-column 120)

;; disable line wrapping
(setq-default truncate-lines t)

; trim trailing whitespaces on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

; select and cut string
(defun str-sel-cut (start end)
  (interactive "r")
  (let ((string (buffer-substring start end)))
    (delete-region start end)
    (kill-new string)))

(str-sel-cut (point) (point-at-eol))
(global-set-key (kbd "C-c x") 'str-sel-cut)

; select and replace string
(defun str-sel-replace ()
  (interactive)
  (if (region-active-p)
      (let ((yanked-string (current-kill 0)))
        (delete-region (region-beginning) (region-end))
        (insert yanked-string))
    (yank)))

(global-set-key (kbd "C-y") 'str-sel-replace)

; spawn split window with an empty buffer
(defun win-split-new (direction)
  (let ((new-buffer (generate-new-buffer "*scratch*")))
    (pcase direction
      ("below" (split-window-below))
      ("right" (split-window-right)))
    (switch-to-buffer new-buffer)))

(global-set-key (kbd "C-x 2") (lambda () (interactive) (win-split-new "below")))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (win-split-new "right")))

; resize split window(v, h) with arrow keys
(defun win-resize-proper (direction)
  (let ((resize-factor 5))
    (pcase direction
      ('up (enlarge-window (- resize-factor)))
      ('down (enlarge-window resize-factor))
      ('left (enlarge-window-horizontally (- resize-factor)))
      ('right (enlarge-window-horizontally resize-factor)))))

(global-set-key (kbd "<C-up>") (lambda () (interactive) (win-resize-proper 'up)))
(global-set-key (kbd "<C-down>") (lambda () (interactive) (win-resize-proper 'down)))
(global-set-key (kbd "<C-left>") (lambda () (interactive) (win-resize-proper 'left)))
(global-set-key (kbd "<C-right>") (lambda () (interactive) (win-resize-proper 'right)))

; other keybindings
(global-set-key (kbd "C-/")      'undo-only)
(global-set-key (kbd "C-?")      'undo-redo)
(global-set-key (kbd "C-c r")    'revert-buffer)
(global-set-key (kbd "M-n")      'mc/edit-lines)
(global-set-key (kbd "<M-down>") 'mc/mark-more-like-this-extended); must press <ESC> after operation.

;; credit: yorickvP on Github
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)
