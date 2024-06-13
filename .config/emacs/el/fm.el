;;; fm.el -*- lexical-binding: t -*-
;;; Commentary: me to her:
;;;             https://www.youtube.com/watch?v=p7j9CYaqIlY&t=3m07s

(defun save-current-directory ()
  (setq saved-directory nil)
  (setq saved-directory default-directory)
  (message "saved directory: %s" saved-directory))

(defun visit-saved-directory ()
  (if saved-directory
      (dired saved-directory)
    (message "no saved directory")))

(defun go-to-previous-directory ()
  (dired ".."))

(defmacro file-book (&rest binds)
  `(progn
     ,@(mapcar (lambda (bind)
              (let ((key (car bind))
                    (action (cadr bind)))
                `(define-key dired-mode-map (kbd ,key)
                    (lambda () (interactive)
                     (let ((arg
                            ,(if (symbolp action)
                                 `(if (boundp ',action)
                                      (symbol-value ',action)
                                                    ',action)
                                                      action)))
                       (if (functionp arg)
                           (funcall arg)
                           (dired arg))))))) binds)))

(with-eval-after-load 'dired
  (file-book
    ("bs" save-current-directory)
    ("bv" visit-saved-directory)
    ("bb" go-to-previous-directory)
    ("be" user-emacs-directory)
    ("ba" "~/")
    ("bl" "~/.local/share/")
    ("bc" "~/.config/")
    ("bd" "~/Downloads/")
    ("bp" "~/Projects/")
    ("bg" "~/git/")))

(defun file-open ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (let* ((file (dired-get-file-for-visit))
           (extension (file-name-extension file))
           (cmd (cond ((member extension '("docx" "pdf"))
                        (list "firefox" file))

                      ((member extension '("jpg" "png" "webp" "gif"))
                        (list "mpv" "--script=/home/irhl/.config/mpv/script-opts/o.lua" file))

                      ((member extension '("mp3" "flac"))
                        (list "foot" "mpv" "--script=/home/irhl/.config/mpv/script-opts/piko.lua" file))

                      ((member extension '("mp4" "mkv" "webm" "wmv"))
                        (list "mpv" file))
                      (t nil))))
      (if cmd
          (apply #'start-process (car cmd) nil (car cmd) (cdr cmd))
        (find-file file)))))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 0)
            (local-set-key (kbd "RET") 'file-open)))
