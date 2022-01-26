;;; this is a comment that should change color here soon.

(defvar-local my-comment-remap-cookie nil
  "Cookie of the last 'face-remap-add-relative'.")

(setq my-comment-remap-cookie (face-remap-add-relative 'font-lock-comment-face 'font-lock-string-face))

;; Now use 'face-remap-remove-relative' on the cookie
(face-remap-remove-relative my-comment-remap-cookie)







;; Let's now write our own face instead of relying on existing ones
;; (those may change and/or they may not suit our particular
;; requirements).

(defface my-comment-remap-style
  '((default :inheret italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#904200")
    (((class color)(min-colors 88) (background dark))
     :foreground "#fba849")
    (t :foreground "yellow"))
    "Yellow-tinted text with slanted font (italics).")

(setq my-comment-remap-style
      (face-remap-add-relative 'font-lock-comment-face 'my-comment-remap-style))

(face-remap-remove-relative my-comment-remap-style)


;; time to put our functionality in a minor-mode so we can activate it
;; whenever we want with M-x
(define-minor-mode my-comment-remap-mode
  "Remap the face of comments."
  :local t
  :init-value nil
  (if my-comment-remap-mode
      (setq my-comment-remap-cookie
            (face-remap-add-relative 'font-lock-comment-face 'my-comment-remap-style))
    (face-remap-remove-relative my-comment-remap-cookie)))

;; Or you can use a hook (this one targets the mode of the *scratch*
;; buffer)
(add-hook 'lisp-interaction-mode-hook #'my-comment-remap-mode)
