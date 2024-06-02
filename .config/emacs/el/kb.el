;;; kb.el -*- lexical-binding: t -*-
;;; Commentary: keybindings will stay here. ^^

(require 'multiple-cursors)
(require 'meow)
(meow-global-mode 1)
(meow-motion-mode 1)
(blink-cursor-mode 0)

(setq meow-expand-yank nil)
(setq meow-leader-key "SPC")
(setq meow-cursor-type-normal 'box)
(setq meow-cursor-type-insert 'hbar)

(defun abort ()
  (interactive)
  (if (active-minibuffer-window)
      (abort-recursive-edit)
    (keyboard-quit)))

(defun incremental-search ()
  (interactive)
  (if isearch-mode
      (isearch-repeat-forward)
    (isearch-forward)))

(defun surround-region ()
  (interactive)
  (let* ((choice (read-char-exclusive
                  "() [] {} <> '' \"\" "))
         (brackets (pcase choice
                     (?\( "()")
                     (?\[ "[]")
                     (?\{ "{}")
                     (?\< "<>")
                     (?\' "''")
                     (?\" "\"\""))))
    (if (region-active-p)
        (progn
          (goto-char (region-end))
          (insert (substring brackets 1))
          (goto-char (region-beginning))
          (insert (substring brackets 0 1))
          (deactivate-mark))
      (insert brackets)
      (backward-char))))

;; unset defaults
(dolist (kb-df '("C-a" "C-b" "C-c" "C-d" "C-e" "C-f" "C-g"
                 "C-h" "C-k" "C-l" "C-n" "C-o" "C-p" "C-q"
                 "C-s" "C-t" "C-v" "C-y" "C-z" "<C-SPC>"))
  (define-key global-map           (kbd kb-df) nil)
  (define-key minibuffer-local-map (kbd kb-df) nil))

;; set kb
(defmacro kb (&rest args)
  (let ((prefix (plist-get args :prefix))
        (normal (plist-get args :normal)))
    `(progn
       ,@(mapcar (lambda (binding)
                   `(define-key (current-global-map)
                      (kbd ,(if prefix (concat prefix " " (car binding)) (car binding)))
                      ',(cdr binding)))
                 normal))))

(with-eval-after-load 'eww
  (kb
   :prefix "C-h"
   :normal
   (("b" . eww-back-url)
    ("r" . eww-reload))))

(kb
 :prefix "C-x"
 :normal
 (("b"          . surround-region)
  ("k"          . describe-key)
  ("f"          . find-file)))

(kb
 :normal
 (("C-s"        . incremental-search)
  ("C-y"        . clipboard-yank)
  ("C-/"        . undo-only)
  ("C-?"        . undo-redo)
  ("<tab>"      . abort)

  ("<M-up>"     . windmove-up)
  ("<M-down>"   . windmove-down)
  ("<M-left>"   . windmove-left)
  ("<M-right>"  . windmove-right)
  ("<S-up>"     . shrink-window)
  ("<S-down>"   . enlarge-window)
  ("<S-left>"   . shrink-window-horizontally)
  ("<S-right>"  . enlarge-window-horizontally)

  ("<M-home>"   . mc/edit-lines)
  ("<M-end>"    . mc/vertical-align-with-space)
  ("<M-prior>"  . mc/unmark-previous-like-this)
  ("<M-next>"   . mc/unmark-next-like-this)
  ("<S-prior>"  . mc/mark-previous-like-this)
  ("<S-next>"   . mc/mark-next-like-this)))

(meow-define-keys 'leader
'("0"        . meow-digit-argument)
'("9"        . meow-digit-argument)
'("8"        . meow-digit-argument)
'("7"        . meow-digit-argument)
'("6"        . meow-digit-argument)
'("5"        . meow-digit-argument)
'("4"        . meow-digit-argument)
'("3"        . meow-digit-argument)
'("2"        . meow-digit-argument)
'("1"        . meow-digit-argument)
'("?"        . meow-cheatsheet))

(meow-define-keys 'normal
'("0"        . meow-expand-0)
'("9"        . meow-expand-9)
'("8"        . meow-expand-8)
'("7"        . meow-expand-7)
'("6"        . meow-expand-6)
'("5"        . meow-expand-5)
'("4"        . meow-expand-4)
'("3"        . meow-expand-3)
'("2"        . meow-expand-2)
'("1"        . meow-expand-1)
'("-"        . negative-argument)
'("'"        . repeat)
'("q"        . meow-quit)
'("gg"       . beginning-of-buffer)
'("G"        . end-of-buffer)
'("w"        . move-beginning-of-line)
'("e"        . move-end-of-line)
'("r"        . open-line)
'("R"        . kill-line)
'("dd"       . kill-whole-line)
'("D"        . meow-backward-delete)
'("i"        . meow-insert)
'("y"        . meow-save)
'("c"        . meow-kill)
'("H"        . meow-grab)
'("h"        . meow-swap-grab)
'("v"        . set-mark-command)
'("V"        . meow-visual-line)
'("x"        . meow-line)
'("X"        . meow-goto-line)
'(";"        . meow-reverse)
'("z"        . meow-pop-selection))
