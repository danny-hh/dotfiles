;;; fl.el -*- lexical-binding: t -*-
;;
;; INSTALL:
;; load the theme as any other .el file.
;;
;; COLOR PICK GUIDE:
;; 1. inspect face property on cursor
;;      C-u C-x =
;;
;; 2. list present faces in current buffer
;;      M-x list-faces-display RET
;;
;;   ,-_,.
;; ,( _  ))
;; 7 (_) 77
;; ((   :))
;;  ~__>~'
;;   cY?'
;;   `l,__
;;    l7-'
;;   ;l
;;   _i_,
;;  l___l
;;  \___/ fl.el (flowershop by irhl)

(unless (>= emacs-major-version 29)
  (error
    "hola payaso fuck you no fudgee barr for you"))

(defvar ext '(emacs-lisp-mode c-mode sh-mode bash-mode))
(dolist (mode ext)
  (font-lock-add-keywords
   mode
   '(("\\_<\\([+-]?[0-9]+\\.?[0-9]*\\)\\_>" 1
      '(face (:foreground "#ebaed4")))

     ("\\<lambda\\>"
      (0 (progn
           (compose-region (match-beginning 0) (match-end 0)
                           (make-char 'greek-iso8859-7 107))
           nil))))))

(defface icon-face
  '((t (:family "FontAwesome"
        :weight bold
        :background  "#f9dcce"
        :foreground  "#bca59b"
        :overline    "#f9dcce"
        :box (:color "#f9dcce"
              :line-width 3
              :style nil))))"")

(defface icon-face-inactive
  '((t (:family "FontAwesome"
        :weight bold
        :background  "#e6ddd7"
        :foreground  "#acaeb1"
        :overline    "#e6ddd7"
        :box (:color "#e6ddd7"
              :line-width 3
              :style nil))))"")

(custom-set-faces
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

 '(hl-line                          ((t (:background "#f5efe8"))))
 '(region                           ((t (:background "#dcd6d3"))))
 '(show-paren-match                 ((t (:background "#f5cac3"))))

 '(isearch
   ((t (:foreground "#faefea"
        :background "#b9b5ae"))))

 '(lazy-highlight
   ((t (:foreground "#b49c84"
        :background "#edd7c2"))))

 '(highlight-indent-guides-character-face
   ((t (:foreground "#f0e8e4"))))

 ;; SECTION: menu
 '(minibuffer-prompt                ((t (:foreground "#9a9392"))))

 ;; SECTION: webpages
 '(highlight                        ((t (:foreground "#f5cac3"))))
 '(browse-url-button                ((t (:foreground "#d3c2bb"))))
 '(button                           ((t (:foreground "#d3c2bb"))))
 '(shr-link                         ((t (:foreground "#9a9392"))))
 '(link                             ((t (:foreground "#84badd"))))

 '(eww-form-text
   ((t :weight bold
       :background  "#efe8e1"
       :box (:color "#5e5958"
             :line-width 2
             :style nil))))

 '(eww-form-textarea
   ((t :weight bold
       :background  "#efe8e1"
       :box (:color "#5e5958"
             :line-width 2
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
        :box (:color "#fcf5ee"
             :line-width 2
             :style nil))))

 '(mode-line
   ((t :weight bold
        :foreground  "#b6a7a0"
        :background  "#fae7de"
        :overline    "#fae7de"
        :box (:color "#fae7de"
             :line-width 2
             :style nil))))

 '(mode-line-inactive
   ((t :weight bold
        :foreground  "#acaeb1"
        :background  "#f0e8e2"
        :overline    "#f0e8e2"
        :box (:color "#f0e8e2"
             :line-width 2
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
