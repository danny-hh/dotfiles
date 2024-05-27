;;; kb.el -*- lexical-binding: t -*-
;;; Commentary: keybindings will stay here. ^^

;; unset defaults
(dolist (key '("\C-a" "\C-b" "\C-c" "\C-d" "\C-e" "\C-f" "\C-g"
               "\C-h" "\C-k" "\C-l" "\C-n" "\C-o" "\C-p" "\C-q"
               "\C-s" "\C-t" "\C-v" "\C-z"))
  (global-unset-key key))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "C-h b") 'eww-back-url)
  (define-key eww-mode-map (kbd "<f5>") 'eww-reload))

(define-key global-map (kbd "C-s")  'abort-recursive-edit)
(define-key global-map (kbd "C-y")  'clipboard-yank)
(define-key global-map (kbd "C-V")  'clipboard-yank)

;; meow's undo and redo behavior isn't linear so no
(define-key global-map (kbd "C-/")  'undo-only)
(define-key global-map (kbd "C-?")  'undo-redo)

(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "<M-up>")      'shrink-window)
(global-set-key (kbd "<M-down>")    'enlarge-window)
(global-set-key (kbd "<M-left>")    'shrink-window-horizontally)
(global-set-key (kbd "<M-right>")   'enlarge-window-horizontally)

(require 'multiple-cursors)
(global-set-key (kbd "C-e")         'mc/edit-lines)
(global-set-key (kbd "<C-down>")    'mc/mark-more-like-this-extended)
(global-set-key (kbd "<C-right>")   'mc/vertical-align-with-space)

(require 'meow)
(meow-global-mode 1)
(meow-motion-mode 0)

(setq meow-expand-yank t)
(setq meow-leader-key "SPC")
(setq meow-cursor-type-normal 'box)
(setq meow-cursor-type-insert 'hbar)

;; anxiety issue
(blink-cursor-mode 0)
