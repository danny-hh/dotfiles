(unless (>= emacs-major-version 29)
  (error
    "hola payaso fuck you no fudgee barr for you motherfucker"))

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

 '(font-lock-builtin-face           ((t (:foreground "#918b8b"))))
 '(font-lock-comment-face           ((t (:foreground "#d2cbc6"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#d2cbc6"))))
 '(font-lock-doc-face               ((t (:foreground "#d2cbc6"))))
 '(font-lock-constant-face          ((t (:foreground "#9cd2bb"))))
 '(font-lock-function-name-face     ((t (:foreground "#bea292"))))
 '(font-lock-keyword-face           ((t (:foreground "#cfbbb0"))))
 '(font-lock-doc-string-face        ((t (:foreground "#d2cbc6"))))
 '(font-lock-string-face            ((t (:foreground "#5e5958"))))
 '(font-lock-type-face              ((t (:foreground "#5e5958"))))
 '(font-lock-variable-name-face     ((t (:foreground "#b3a7a9"))))
 '(font-lock-preprocessor           ((t (:foreground "#b3a7a9"))))
 '(font-lock-warning-face           ((t (:foreground "#5e5958"))))

 '(hl-line                          ((t (:background "#F5EFE8"))))
 '(region                           ((t (:background "#f0e4dd"))))
 '(show-paren-match                 ((t (:background "#f5cac3"))))
 '(vertical-border                  ((t (:foreground "#f3ebeb"))))

 '(mode-line                        ((t (:foreground "#7c7773"
                                         :background "#f4e7e1"))))
 '(mode-line-inactive               ((t (:foreground "#9d9ea0"
                                         :background "#dfcfc5"))))
  (set-face-attribute 'header-line nil   :underline nil)

 '(default                          ((t (:foreground "#5e5958"
                                         :background "#fcf5ee"))))
 '(warning                          ((t (:foreground "#5e5958"
                                         :weight bold))))
 ;; end of highlight groups
 )

(provide-theme 'untitled)
