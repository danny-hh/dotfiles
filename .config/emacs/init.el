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
                 `(use-package ,pkg :ensure t))
               packages)))

(install-packages
 evil
 multiple-cursors
 rainbow-mode
 rainbow-delimiters
 fontawesome)

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

(setq inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil)

(setq use-short-answers t); yes, no -> y, n

(defun display-startup-echo-area-message ()
  (let ((user-name (getenv "USER")))
    (message "hi %s, happy hacking!" user-name)))

(setq default-frame-alist
       (append (list '(tool-bar-lines . 0)
		     '(menu-bar-lines . 0)
                     '(left-fringe . 0)
                     '(right-fringe . 0)
		     '(internal-border-width . 25)
		     '(vertical-scroll-bars . nil)
		     '(font . "Comic Code Medium 9"))))

(setq window-divider-default-right-width 4
      window-divider-default-places 'right-only)

(window-divider-mode 1)

(show-paren-mode 1)
(global-hl-line-mode)

(setq fill-column 120
      tab-width 4)

;; disable line wrap, prefer spaces over tabs
(setq-default truncate-lines t
              indent-tabs-mode nil)

;; remove dollar sign at the end of truncated lines
(set-display-table-slot standard-display-table 0 ?\ )

;; trim trailing whitespaces on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; window management: close windows with unmodified buffers
(defun win-close ()
  "M-x win-close RET"
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (buffer-modified-p buffer)
                (string-prefix-p " *Minibuf" (buffer-name buffer)))
      (let ((window (get-buffer-window buffer)))
        (when window
          (delete-window window)))
      (kill-buffer buffer))))

;; window management: split, focus, resize
(defmacro define-key-bindings (&rest bindings)
  `(progn
     ,@(mapcar (lambda (binding)
                 (let ((key (car binding))
                       (fn (intern (concat "win-" (symbol-name (cadr binding)))))
                       (arg (caddr binding)))
                   `(global-set-key (kbd ,key) (lambda () (interactive) (,fn ',arg)))))
               bindings)))

(defun win-split-new (direction)
  (let ((new-buffer (generate-new-buffer "*scratch*")))
    (pcase direction
      ('below (split-window-below))
      ('right (split-window-right)))
    (switch-to-buffer new-buffer)))

(defun win-focus-change (direction)
  (pcase direction
    ('up (windmove-up))
    ('down (windmove-down))
    ('left (windmove-left))
    ('right (windmove-right))))

(defun win-resize-proper (direction)
  (let ((resize-factor 5))
    (pcase direction
      ('up (enlarge-window (- resize-factor)))
      ('down (enlarge-window resize-factor))
      ('left (enlarge-window-horizontally (- resize-factor)))
      ('right (enlarge-window-horizontally resize-factor)))))

(define-key-bindings
 ("C-x 2"       split-new     below)
 ("C-x 3"       split-new     right)
 ("C-x <up>"    focus-change  up)
 ("C-x <down>"  focus-change  down)
 ("C-x <left>"  focus-change  left)
 ("C-x <right>" focus-change  right)
 ("<M-up>"      resize-proper up)
 ("<M-down>"    resize-proper down)
 ("<M-left>"    resize-proper left)
 ("<M-right>"   resize-proper right))

(require 'multiple-cursors)
  :config
  (global-set-key (kbd "M-n") 'mc/edit-lines)
  (global-set-key (kbd "<C-down>") 'mc/mark-more-like-this-extended); must press <ESC> after spawn.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((css-mode emacs-lisp-mode) . rainbow-mode))

(load-file "~/.config/emacs.d/evil.el")

(add-to-list 'custom-theme-load-path "~/.config/emacs.d/themes/")
(load-theme 'untitled t)

;; my fudgee barr
(setq-default mode-line-format nil)

(defun evil-mode-state ()
  (cond
   ((evil-normal-state-p)   "NO")
   ((evil-visual-state-p)   "VI")
   ((evil-insert-state-p)   "I")
   ((evil-emacs-state-p)    "E")
   ((evil-motion-state-p)   "M")
   ((evil-operator-state-p) "O")
   (t "?")))

(defun fudgee-barr ()
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (let* ((str-right (format "Ln %d, Col %d     " ; <-- The reason why we are putting spaces here
                                (line-number-at-pos) ; is to align the Ln and Col indicator properly.
                                (current-column)))   ; Font Awesome has unicode icons that makes up
                                                     ; the extra spaces that don't go along with the
                                                     ; ascii code and it messes with the normal alignment.
             (icon (if (eq window (selected-window))
                       (propertize " " 'face 'icon-face)
                       (propertize " " 'face 'icon-face-inactive)))

             (inon (if (eq window (selected-window))
                       (propertize "    " 'face 'icon-face)            ; Why not add the spaces directly
                       (propertize "    " 'face 'icon-face-inactive))) ; inside the icons? I cannot.
                                                                       ; I don't need to tell why.
             (file (if (eq major-mode 'eww-mode)
                       (let ((url (plist-get eww-data :url)))                    ; It's better to just change the file name
                         (if url (concat "URL: " url)                            ; to the url address than completely using
                           "EWW"))                                               ; eww's own header property that replaces
                     (file-name-nondirectory (or buffer-file-name "untitled")))) ; the custom header line.

             (str-left  (format " %s [%s] (#%s) (%s%s%s)"
                                    file
                                    (evil-mode-state)
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

             (macapuno (concat icon
                               inon
                               str-left-p
                               str-cents-p
                               str-right-p)))

        (setq-local header-line-format macapuno)))))

(add-hook 'window-configuration-change-hook 'fudgee-barr)
(add-hook 'post-command-hook 'fudgee-barr)

;; org mode
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-postamble nil)

;; eww stuff
(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "C-x b") 'eww-back-url)
  (define-key eww-mode-map (kbd "<f5>") 'eww-reload))

(defun enter (url)
  (interactive)
  (eww url))

(defun boo ()
  "i don't know how to use the builtin bookmarks, i find my own easier."
  (interactive)
  (let ((items
         '(("emacs"      . "http://web.archive.org/web/20070808235903/https://www.gnu.org/software/emacs/")
           ("emacs girl" . "https://4chanarchives.com/board/k/thread/29900098")
           ("g"          . "https://boards.4chan.org/g/")
           ("fa"         . "https://boards.4chan.org/fa/")
           ("his"        . "https://boards.4chan.org/his/")
           ("furret"     . "https://e621.net/posts?page=4&tags=ke_mo_suke")
           ("x"          . nil))))

    (let* ((selected-item (completing-read ":// " (mapcar #'car items)))
           (given-url (cdr (assoc selected-item items))))
      (if given-url
          (enter given-url)
        (message "x")))))

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
