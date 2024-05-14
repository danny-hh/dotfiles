(deftheme untitled
  "untitled by irhl")

(defvar ext '(emacs-lisp-mode c-mode sh-mode bash-mode)
  "syntax highlighting for numbers")

(dolist (mode ext)
  (font-lock-add-keywords
   mode
   '(("\\_<\\([+-]?[0-9]+\\.?[0-9]*\\)\\_>" 1
      '(face (:foreground "#ebaed4"))))))

(custom-theme-set-faces
 'untitled

 '(rainbow-delimiters-depth-1-face  ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-2-face  ((t (:foreground "#ebc3a9"))))
 '(rainbow-delimiters-depth-3-face  ((t (:foreground "#84badd"))))
 '(rainbow-delimiters-depth-4-face  ((t (:foreground "#e889c6"))))
 '(rainbow-delimiters-depth-5-face  ((t (:foreground "#6ebb77"))))
 '(rainbow-delimiters-depth-6-face  ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-7-face  ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-8-face  ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-9-face  ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-10-face ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-11-face ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-12-face ((t (:foreground "#c79d83"))))

 '(font-lock-builtin-face           ((t (:foreground "#5e5958"))))
 '(font-lock-comment-face           ((t (:foreground "#d2cbc6"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#d2cbc6"))))
 '(font-lock-doc-face               ((t (:foreground "#5e5958"))))
 '(font-lock-constant-face          ((t (:foreground "#918b8b"))))
 '(font-lock-function-name-face     ((t (:foreground "#5e5958"))))
 '(font-lock-keyword-face           ((t (:foreground "#5e5958"))))
 '(font-lock-string-face            ((t (:foreground "#5e5958"))))
 '(font-lock-type-face              ((t (:foreground "#5e5958"))))
 '(font-lock-variable-name-face     ((t (:foreground "#5e5958"))))
 '(font-lock-warning-face           ((t (:foreground "#5e5958"))))

 '(hl-line                          ((t (:background "#F5EFE8"))))
 '(region                           ((t (:background "#f0e4dd"))))
 '(show-paren-match                 ((t (:background "#f5cac3"))))

 '(vertical-border                  ((t (:foreground "#f3ebeb"))))

 '(default            ((t (:foreground "#5e5958" :background "#fcf5ee"))))
 '(warning            ((t (:foreground "#77706e" :weight bold))))

 '(mode-line          ((t (:foreground "#7c7773" :background "#e2e8f2"))))
 '(mode-line-inactive ((t (:foreground "#9d9ea0" :background "#dfcfc5"))))

 ;; end of highlight groups
 )

(provide-theme 'untitled)
