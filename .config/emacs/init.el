;;; init.el -*- lexical-binding: t -*-
;;; Commentary: my lisp machine configuration.

;; a garbage truck for fast startup time
(setq gc-cons-threshold (* 50 1000 1000))

(setq create-lockfiles nil
      auto-save-default nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t)

(let ((backup-dir (expand-file-name (concat user-emacs-directory "backups/"))))
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))))

(prefer-coding-system 'utf-8)

;; enable local variable check
(setq enable-local-eval 'maybe
      enable-local-variables t)

;; yes or no -> y or n
(setq use-short-answers t)

(setq inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil)

(defun display-startup-echo-area-message ()
  (let ((user-name (getenv "USER")))
    (message "hi %s, happy hacking!" user-name)))

(menu-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode)

(setq fill-column 120
      tab-width 4)

;; disable line wrap, prefer spaces over tabs
(setq-default truncate-lines t
              indent-tabs-mode nil)

;; trim trailing whitespaces on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; select and cut string
(defun str-sel-cut (start end)
  (interactive "r")
  (let ((string (buffer-substring start end)))
    (delete-region start end)
    (kill-new string)))

(str-sel-cut (point) (point-at-eol))
(global-set-key (kbd "C-d") 'str-sel-cut)

;; select and replace string
(defun str-sel-replace ()
  (interactive)
  (if (region-active-p)
      (let ((yanked-string (current-kill 0)))
        (delete-region (region-beginning) (region-end))
        (insert yanked-string))
    (yank)))

(global-set-key (kbd "C-y") 'str-sel-replace)

;; close windows with unmodified buffers: 'M-x win-close RET'
(defun win-close ()
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (buffer-modified-p buffer)
                (string-prefix-p " *Minibuf" (buffer-name buffer)))
      (let ((window (get-buffer-window buffer)))
        (when window
          (delete-window window)))
      (kill-buffer buffer))))

;; empty split window
(defun win-split-new (direction)
  (let ((new-buffer (generate-new-buffer "*scratch*")))
    (pcase direction
      ('below (split-window-below))
      ('right (split-window-right)))
    (switch-to-buffer new-buffer)))

;; quick focus window
(defun win-focus-change (direction)
  (pcase direction
    ('up (windmove-up))
    ('down (windmove-down))
    ('left (windmove-left))
    ('right (windmove-right))))

;; quick resize window
(defun win-resize-proper (direction)
  (let ((resize-factor 5))
    (pcase direction
      ('up (enlarge-window (- resize-factor)))
      ('down (enlarge-window resize-factor))
      ('left (enlarge-window-horizontally (- resize-factor)))
      ('right (enlarge-window-horizontally resize-factor)))))

(global-set-key (kbd "C-x 2")       (lambda () (interactive) (win-split-new     'below)))
(global-set-key (kbd "C-x 3")       (lambda () (interactive) (win-split-new     'right)))
(global-set-key (kbd "C-x <up>")    (lambda () (interactive) (win-focus-change  'up)))
(global-set-key (kbd "C-x <down>")  (lambda () (interactive) (win-focus-change  'down)))
(global-set-key (kbd "C-x <left>")  (lambda () (interactive) (win-focus-change  'left)))
(global-set-key (kbd "C-x <right>") (lambda () (interactive) (win-focus-change  'right)))
(global-set-key (kbd "<M-up>")      (lambda () (interactive) (win-resize-proper 'up)))
(global-set-key (kbd "<M-down>")    (lambda () (interactive) (win-resize-proper 'down)))
(global-set-key (kbd "<M-left>")    (lambda () (interactive) (win-resize-proper 'left)))
(global-set-key (kbd "<M-right>")   (lambda () (interactive) (win-resize-proper 'right)))

;; map C-c to delete windows quicker
(global-set-key (kbd "C-c") 'delete-window)

(require 'multiple-cursors)
  :config
  (global-set-key (kbd "M-n") 'mc/edit-lines)
  (global-set-key (kbd "<C-down>") 'mc/mark-more-like-this-extended); must press <ESC> after spawn.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((css-mode emacs-lisp-mode) . rainbow-mode))

;; evil-mode :D
(load-file "~/.config/emacs.d/evil.el")

;; load user themes
(add-to-list 'custom-theme-load-path "~/.config/emacs.d/themes/")
(load-theme 'untitled t)

;; statusline configuration
(setq-default mode-line-format nil)

(defun evil-mode-state ()
  (cond
   ((evil-normal-state-p)   "NO")
   ((evil-visual-state-p)   "VI")
   ((evil-insert-state-p)   "I")
   ((evil-emacs-state-p)    "E")
   ((evil-motion-state-p)   "M")
   ((evil-operator-state-p) "O")
   (t "[?]")))

(setq-default header-line-format
  '(:eval
    (let* ((file-name (or buffer-file-name "Untitled"))
           (str-right (format " Ln %d, Col %d"
                              (line-number-at-pos)
                              (current-column)))

           (str-left  (format " %s {%s} [%s] (%s%s%s)"
                              (file-name-nondirectory file-name)
                              (if vc-mode (substring vc-mode 5) "b?")
                              (evil-mode-state)
                              (format-mode-line mode-name)
                              (if abbrev-mode " ABBREV" "")
                              (if eldoc-mode " ELDOC" "")))

           (str-right-p  (propertize str-right 'face 'mode-line-buffer-id))
           (str-left-p   (propertize str-left  'face 'mode-line-buffer-id))

           (fill-length  (- (window-width)
                            (length str-left-p)
                            (length str-right-p) 1)))

      (concat str-left-p (make-string (max fill-length 0) ?\s) str-right-p))))

(add-hook 'post-command-hook 'force-mode-line-update)

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
