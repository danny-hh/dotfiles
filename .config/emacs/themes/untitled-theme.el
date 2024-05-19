(unless (>= emacs-major-version 29)
  (error
    "hola payaso fuck you no fudgee barr for you"))

(deftheme untitled
  "untitled by irhl")

(defvar ext '(emacs-lisp-mode c-mode sh-mode bash-mode)
  "syntax highlighting for numbers")

(dolist (mode ext)
  (font-lock-add-keywords
   mode
   '(("\\_<\\([+-]?[0-9]+\\.?[0-9]*\\)\\_>" 1
      '(face (:foreground "#ebaed4"))))))

(defface icon-face
  '((t (:family "FontAwesome"
        :weight bold
        :foreground  "#b8aaa5"
        :background  "#faefea"
        :overline    "#faefea"
        :box (:color "#faefea"
              :line-width 6
              :style nil))))"")

(defface icon-face-inactive
  '((t (:family "FontAwesome"
        :weight bold
        :foreground  "#acaeb1"
        :background  "#ebe2dc"
        :overline    "#ebe2dc"
        :box (:color "#ebe2dc"
              :line-width 6
              :style nil))))"")

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

 '(vertical-border                  ((t (:foreground "#f3ebeb"))))
 '(window-divider                   ((t (:foreground "#f5efe8"))))
 '(window-divider-first-pixel       ((t (:foreground "#f5efe8"))))
 '(window-divider-last-pixel        ((t (:foreground "#f5efe8"))))

 '(hl-line                          ((t (:background "#F5EFE8"))))
 '(region                           ((t (:background "#f0e8e4"))))
 '(show-paren-match                 ((t (:background "#f5cac3"))))

 ;; SECTION: webpages
 '(highlight                        ((t (:foreground "#f5cac3"))))
 '(browse-url-button                ((t (:foreground "#d3c2bb"))))
 '(button                           ((t (:foreground "#d3c2bb"))))
 '(shr-link                         ((t (:foreground "#9a9392"))))

 '(eww-form-text
   ((t :weight bold
       :background  "#efe8e1"
       :box (:color "#5e5958"
             :line-width 1
             :style nil))))

 '(eww-form-textarea
   ((t :weight bold
       :background  "#efe8e1"
       :box (:color "#5e5958"
             :line-width 1
             :style nil))))

 '(eww-invalid-certificate
   ((t (:foreground "#f5cac3"
        :inherit bold))))
 '(eww-valid-certificate
   ((t (:foreground "#d3c2bb"
        :inherit bold))))

 ;; SECTION: fudgee barr
 '(header-line
   ((t :weight bold
       :background  "#f4e7e1"
       :foreground  "#d3c2bb"
       :overline    "#fcf5ee"
       :box (:color "#f4e7e1"
             :line-width 6
             :style nil))))

 '(mode-line
   ((t :weight bold
       :background  "#f4e7e1"
       :foreground  "#d3c2bb"
       :overline    "#fcf5ee"
       :box (:color "#f4e7e1"
             :line-width 6
             :style nil))))

 '(mode-line-inactive
   ((t :weight bold
       :background  "#e2d8d2"
       :foreground  "#acaeb1"
       :overline    "#fcf5ee"
       :box (:color "#e2d8d2"
             :line-width 5
             :style nil))))

 '(header-line-highlight
   ((t :box (:color nil))))

 '(mode-line-highlight
   ((t :box (:color nil))))

 ;; SECTION: defaults
 '(warning
   ((t (:foreground "#5e5958"
        :weight bold))))
 '(default
   ((t (:foreground "#5e5958"
        :background "#fcf5ee")))))

(provide-theme 'untitled)
