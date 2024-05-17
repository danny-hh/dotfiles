(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

(eval-after-load 'evil-maps
  '(progn
     (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
     (define-key evil-motion-state-map (kbd "C-a") 'evil-beginning-of-line)))
