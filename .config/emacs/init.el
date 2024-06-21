;;; init.el -*- lexical-binding: t -*-
;;; Commentary: my lisp machine conf

(defun report()
  (interactive)
  (let* ((buffers (seq-filter
                    (lambda (name) (not (string-match "^ " name)))
                    (mapcar 'buffer-name (buffer-list))))
         (buffer (ido-completing-read "buffer: " buffers)))
    (save-excursion
      (with-current-buffer buffer
        (clipboard-kill-ring-save (point-min) (point-max))
        (message "y+ %s" buffer)))))

;; C:\Users\User\AppData\Roaming\.emacs.d
;; $HOME/.emacs.d
(setq default-directory user-emacs-directory
      custom-file "/dev/null") ; tu puta madre

(defun reload ()
  (interactive)
  (let ((config-dir (expand-file-name "el/" user-emacs-directory)))
    (mapc (lambda (file)
            (let ((file-path (expand-file-name file config-dir)))
              (when (file-readable-p file-path)
                          (load-file file-path)))) '("fl.el"
                                                     "fm.el"
                                                     "kb.el"
                                                     "dired-nnn.el"))))
;; require 'package is automatically called by use-package
;; package initialize isn't needed over v27
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa"  . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defmacro install-packages (&rest packages)
  `(progn
     ,@(mapcar (lambda (pkg)
                 (if (consp pkg)
                     `(use-package ,(car pkg) ,@(cdr pkg)
                        :ensure t))) packages)
     ;; load files after checking packages
     (reload)))

(install-packages
 (fontawesome)
 (svg-lib)
 (svg-tag-mode)
 (lua-mode)
 (rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
 (rainbow-mode       :hook ((css-mode
                             lua-mode
                             emacs-lisp-mode) . rainbow-mode))
 (multiple-cursors)
 (smartparens        :hook ((prog-mode . smartparens-mode)
                            (lisp-mode . smartparens-strict-mode)))
 (undo-fu)
 (undo-fu-session    :after undo-fu
                     :config (setq undo-fu-session-directory (expand-file-name "hist-undo" user-emacs-directory))
                             (setq undo-fu-session-file-limit nil)
                             (setq undo-fu-session-linear t)
                             (undo-fu-session-global-mode)))
;; conf downtown
(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

(defun export ()
  (interactive)
  (if-let ((file buffer-file-name))
      (let* ((backup-dir  (expand-file-name "backups/" user-emacs-directory))
             (backup-name (read-string      "file-name: " (file-name-nondirectory file)))
             (backup-path (expand-file-name  backup-name backup-dir)))
        (make-directory backup-dir t)
        (copy-file file backup-path t)
        (message "file exported to %s" backup-path))
    (message "buffer is not associated with a file")))

(prefer-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)

;; editor greetings
(defun display-startup-echo-area-message ()
  (let ((user-name (getenv "USER")))
    (message "hi %s, happy hacking!" user-name)))

(setq inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil
      confirm-nonexistent-file-or-buffer nil)

;; line by line scrolling
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 10
      scroll-preserve-screen-position t)

(setq default-frame-alist
      '((font . "Iosevka Comfy Medium 10")
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (left-fringe . 0)
        (right-fringe . 0)
        (internal-border-width . 25)
        (vertical-scroll-bars . nil)
        (window-divider-default-right-width . 4)
        (window-divider-default-places . right-only)))

(window-divider-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(show-paren-mode 1)
(delete-selection-mode 1)

;; hide cursor on inactive windows
(setq-default cursor-in-non-selected-windows nil)

(setq-default truncate-lines t
              indent-tabs-mode nil)

;; remove dollar sign at the end of truncated lines
(set-display-table-slot standard-display-table 0 ?\ )
(setq tab-width 4)

;; trim trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; org-mode stuff
(setq org-fontify-whole-block-delimiter-line t
      org-fontify-whole-heading-line t
      org-fontify-todo-headline t
      org-fontify-done-headline t
      org-cycle-level-faces nil
      org-adapt-indentation nil)

(setq svg-tag-tags
  '(("|[0-9a-zA-Z- ]+?|" . ((lambda (tag) (svg-tag-make tag
                                      :face 'font-lock-comment-face
                                      :font-family "Iosevka Comfy Medium"
                                      :font-size 9
                                      :radius 0
                                      :beg 1 :end -1))))))

(add-hook 'org-mode-hook 'svg-tag-mode 1)

;; eww stuff
(defvar url-address
  '(("emacs"      . "http://web.archive.org/web/20070808235903/https://www.gnu.org/software/emacs/")
    ("emacs girl" . "https://4chanarchives.com/board/k/thread/29900098")
    ("g"          . "https://boards.4chan.org/g/")
    ("fa"         . "https://boards.4chan.org/fa/")
    ("his"        . "https://boards.4chan.org/his/")
    ("furret"     . "https://e621.net/posts?page=4&tags=ke_mo_suke")))

(defun docs()
  (interactive)
  (let ((buffer-name "*docs*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "\nyou're fucking gay \n\n")
      (let ((max-title-length (apply 'max (mapcar #'length
                                          (mapcar #'car url-address)))))
        (dolist (item url-address)
          (let ((url-title (car item))
                (url       (cdr item)))
            (insert-button url-title 'action (lambda (_) (eww url)))
            (insert (make-string (- (+ 2 max-title-length)
                                    (length url-title)) ?\s))
            (insert (format "%s\n" url))
            (insert (make-string (window-width) ?-))
            (insert "\n")))))
    (switch-to-buffer buffer-name)))

;; my fudgee barr
(setq-default mode-line-format nil)

(defun fudgee-barr ()
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (let* ((str-right (format "Ln %d, Col %d    "
                                (line-number-at-pos)
                                (current-column)))

             (file-name (if (eq major-mode 'eww-mode)
                            (let ((url (plist-get eww-data :url)))
                               (if url (concat "URL: " url)
                                 "EWW"))
                            (file-name-nondirectory
                               (or buffer-file-name "Untitled"))))

             (str-left (format " %s [%s] (#%s) (%s%s%s)"
                               file-name
                               (if (boundp 'edit-state) edit-state "OTHERS")
                               (if vc-mode (substring vc-mode 5) "?")
                               (format-mode-line mode-name)
                               (if abbrev-mode " ABBREV" "")
                               (if eldoc-mode " ELDOC" "")))

             (fill-length (max 0 (- (window-width window)
                                    (length str-left)
                                    (length str-right) 0)))

             (face (if (eq window (selected-window))
                       'mode-line
                       'mode-line-inactive))

             (icon (if (eq window (selected-window))
                       (propertize "    " 'face 'icon-face)
                       (propertize "    " 'face 'icon-face-inactive)))

             (str-left-p  (propertize str-left 'face face))
             (str-right-p (propertize str-right 'face face))
             (str-cents-p (propertize (make-string fill-length ?\s) 'face face))
             (x (concat icon
                        str-left-p
                        str-cents-p
                        str-right-p)))
            (setq-local header-line-format x)))))

(add-hook 'window-configuration-change-hook 'fudgee-barr)
(add-hook 'post-command-hook 'fudgee-barr)
