;;; fm.el -*- lexical-binding: t -*-
;;; Commentary: me to her:
;;;             https://www.youtube.com/watch?v=p7j9CYaqIlY&t=3m07s

(defun create-new-file ()
  (let* ((dir (dired-current-directory))
         (new-file-name (read-string "[path/]name: ")))
    (write-region "" nil (expand-file-name new-file-name dir))
    (revert-buffer)))

(defun save-current-directory ()
  (setq saved-directory nil)
  (setq saved-directory default-directory)
  (message "saved directory: %s" saved-directory))

(defun visit-saved-directory ()
  (if saved-directory
      (dired saved-directory)
    (message "no saved directory")))

(defun go-to-parent-directory ()
  (dired ".."))

(defmacro file-book (&rest binds)
  `(progn
     ,@(mapcar (lambda (bind)
                 `(define-key dired-mode-map (kbd ,(car bind))
                    ,(if (symbolp (cadr bind))
                         `(lambda () (interactive) (,(cadr bind)))
                       `(lambda () (interactive) (dired ,(cadr bind))))))
               binds)))

(with-eval-after-load 'dired
  (file-book
    ("ba" "~/")
    ("bl" "~/.local/share/")
    ("bc" "~/.config/")
    ("bd" "~/Downloads/")
    ("bp" "~/Projects/")
    ("bg" "~/git/")
    ("be" "~/.config/emacs.d/")
    ("bs" save-current-directory)
    ("bv" visit-saved-directory)
    ("bb" go-to-parent-directory)
    ("bn" create-new-file)))

(defun file-open ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (let* ((file (dired-get-file-for-visit))
           (extension (file-name-extension file))
           (cmd (cond ((member extension '("mp3" "flac"))
                       (list "foot" "mpv" "--script=/home/irhl/.config/mpv/script-opts/piko.lua" file))

                      ((member extension '("html" "adoc" "docx" "pdf"))
                       (list "firefox" file))

                      ((member extension '("jpg" "png" "webp" "gif"))
                        (list "mpv" file))
                      (t nil))))
      (if cmd
          (apply #'start-process (car cmd) nil (car cmd) (cdr cmd))
        (find-file file)))))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
            (local-set-key (kbd "RET") 'file-open)))
