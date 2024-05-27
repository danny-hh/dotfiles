;;; init.el -*- lexical-binding: t -*-
;;; Commentary: my lisp machine configuration.

(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defmacro install-packages (&rest packages)
  `(progn
     ,@(mapcar (lambda (pkg)
                 (if (consp pkg)
                     `(use-package ,(car pkg) ,@(cdr pkg)
                        :ensure t))) packages)))
(install-packages
 (meow)
 (multiple-cursors)
 (fontawesome)
 (highlight-indent-guides :config (setq highlight-indent-guides-method 'character
                                        highlight-indent-guides-auto-enabled nil)
                            :hook (prog-mode . highlight-indent-guides-mode))
 (rainbow-delimiters        :hook (prog-mode . rainbow-delimiters-mode))
 (rainbow-mode              :hook ((css-mode emacs-lisp-mode) . rainbow-mode)))

(defun load-user-config (file)
  (load-file (expand-file-name file "~/.config/emacs.d/")))
  (mapc 'load-user-config '("el/fl.el"
                            "el/fm.el"
                            "el/kb.el"))
(defun reload ()
  (interactive)
  (mapc 'load-user-config '("init.el"
                            "el/fl.el"
                            "el/fm.el"
                            "el/kb.el")))
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

;; assign clipboard
(defun wl-copy (text)
  (let ((p (make-process :name "wl-copy"
                         :command '("wl-copy")
                         :connection-type 'pipe)))
    (process-send-string p text)
    (process-send-eof p)))
(setq interprogram-cut-function 'wl-copy)

;; editor greetings
(defun display-startup-echo-area-message ()
  (let ((user-name (getenv "USER")))
    (message "hi %s, happy hacking!" user-name)))

;; yes, no -> y, n
(setq use-short-answers t)

(setq inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil)

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

(show-paren-mode 1)
(global-hl-line-mode)
(delete-selection-mode 1)

;; disable line wrap, prefer spaces over tabs
(setq fill-column 120
      tab-width 4)

(setq-default truncate-lines t
              indent-tabs-mode nil)

;; remove dollar sign at the end of truncated lines
(set-display-table-slot standard-display-table 0 ?\ )

;; trim trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; EWW EEEEEE!!!!!!!!!!!!!!!!!! THATS POOOOPP!!!
(defun docst()
  (interactive)
  (if (eq major-mode 'eww-mode)
      (org-mode)
    (eww-mode)))

(defvar url-address
  '(("emacs"      . "http://web.archive.org/web/20070808235903/https://www.gnu.org/software/emacs/")
    ("emacs girl" . "https://4chanarchives.com/board/k/thread/29900098")
    ("t"          . "https://boards.4chan.org/g/")
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
      (let* ((str-right (format "Ln %d, Col %d    "  ; <-- The reason why we are putting spaces here
                                (line-number-at-pos) ; is to align the Ln and Col indicator properly.
                                (current-column)))   ; Font Awesome has unicode icons that makes up
                                                     ; the extra spaces that don't go along with the
             (icon (if (eq window (selected-window)) ; ascii code and it messes with the normal alignment.
                       (propertize "    " 'face 'icon-face)
                       (propertize "    " 'face 'icon-face-inactive)))

             (file (if (eq major-mode 'eww-mode)
                       (let ((url (plist-get eww-data :url)))                    ; It's better to just change the file name
                         (if url (concat "URL: " url)                            ; to the url address than completely using
                           "EWW"))                                               ; eww's own header property that replaces
                     (file-name-nondirectory (or buffer-file-name "Untitled")))) ; the custom header line.

             (str-left  (format " %s [%s] (#%s) (%s%s%s)"
                                file
                                (if overwrite-mode "o" "n")
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
