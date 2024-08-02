;;; ci.el -*- lexical-binding: t -*-
;;; Commentary: compilation instructions

(defun compile ()
  (interactive)
  (let* ((file-name         (buffer-file-name))
         (file-base-name    (file-name-sans-extension (file-name-nondirectory file-name)))
         (default-directory (file-name-directory file-name)))
    (compile (format "make %s && ./%s"
                     file-base-name file-base-name))))

(global-set-key (kbd "<f5>") 'compile)
