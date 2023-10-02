(message "dark-mode off.")

(setq my:use-theme 'doom-one-light)

;; init.el line #138
;; I find it much easier to use underline rather than actual
;; highlighting to read.
(defun my:set-custom-faces()
  "Set custom faces after the theme is loaded."
  (custom-set-faces
   '(ivy-current-match
     ((t (:background nil :underline t))))
   '(company-tooltip
     ((t (:background nil))))
   '(company-tooltip-selection
     ((t (:background nil :underline t))))
   '(highlight
     ((t (:underline t))))
                                        ;((t (:background "nil" :foreground "nil" :underline t))))
   )

  ;; Customize powerline colors. I like purple pink-ish
  (custom-set-faces
   '(powerline-active1
     ((t (:background "white" :foreground "#9e51b5" :bold t :underline t)))) ;; was #c678dd
   '(powerline-active2
     ((t (:background "white" :foreground "#9e51b5" :bold t :underline t)))) ;; was #c678dd
   )

  (set-face-attribute 'powerline-inactive1 nil :background
                      (face-attribute 'powerline-active1 :background))
  (set-face-attribute 'powerline-inactive2 nil :background
                      (face-attribute 'powerline-active2 :background))
  (set-face-attribute 'powerline-inactive1 nil :foreground
                      (face-attribute 'powerline-active1 :foreground))
  (set-face-attribute 'powerline-inactive2 nil :foreground
                      (face-attribute 'powerline-active2 :foreground))

  ;; Custom face for avy
  (custom-set-faces
   '(avy-lead-face
     ((t (:foreground "white" :background "#9e51b5" :bold t :underline t))))) ;; was #c678dd

  ;; Make the background pure white because I find that easier to read
  (custom-set-faces
   '(default
      ((t (:background "white")))))
  )




;; init.el line #2188
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string-equal my:use-theme "darcula")
  (use-package jetbrains-darcula-theme
    :ensure t
    :config
    (load-theme 'jetbrains-darcula t)))

(when (string-match "doom-*" (symbol-name my:use-theme))
  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)

    ;; if nil, bold is universally disabled
    (setq doom-themes-enable-bold t)
    ;; if nil, italics is universally disabled
    (setq doom-themes-enable-italic t)
    ;; Load the selected theme
    (load-theme my:use-theme t)

    (require 'doom-themes-ext-org)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    )
  )

(when (string-equal my:use-theme "spacemacs-dark")
  (use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-dark t)))

(when (string-equal my:use-theme "sourcerer")
  (use-package sourcerer-theme
    :ensure t
    :config
    (load-theme 'sourcerer t))

  (set-face-background 'hl-line "#372E2D")
  ;; The minibuffer default colors with my theme are impossible to read,
  ;; so change them to something better using ivy-minibuffer-match-face.

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default
      ((((type tty) (background dark)) (:background "nil"))))
   '(company-preview
     ((t (:background "#073642" :foreground "#2aa198"))))
   '(company-preview-common
     ((t (:foreground "#93a1a1" :underline t))))
   '(company-scrollbar-bg
     ((t (:background "#073642" :foreground "#2aa198"))))
   '(company-scrollbar-fg
     ((t (:foreground "#002b36" :background "#839496"))))
   '(company-template-field
     ((t (:background "#7B6000" :foreground "#073642"))))
   '(company-tooltip
     ((t (:background "light green" :foreground "DeepSkyBlue1"))))
   '(company-tooltip-annotation
     ((t (:foreground "#93a1a1" :background "#073642"))))
   '(company-tooltip-common
     ((t (:foreground "#93a1a1" :underline t))))
   '(company-tooltip-common-selection
     ((t (:foreground "#93a1a1" :underline t))))
   '(company-tooltip-mouse
     ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
   '(company-tooltip-selection
     ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
   '(header-line
     ((t (:background "#003366")))))
  (when my:use-ivy
    (custom-set-faces
     '(ivy-minibuffer-match-face-1
       ((((class color) (background light)) (:background "#555555"))
        (((class color) (background dark)) (:background "#555555"))))
     '(ivy-minibuffer-match-face-2
       ((t (:background "#314f30" :weight bold))))
     '(ivy-minibuffer-match-face-3
       ((t (:background "#48225b" :weight bold))))
     '(ivy-minibuffer-match-face-4
       ((t (:background "#680a0a" :weight bold))))
     '(which-func ((t (:foreground "#8fb28f"))))))
)

;; I don't care to see the splash screen
(setq inhibit-splash-screen t)

;; Hide the scroll bar
(scroll-bar-mode -1)
;; Make mode bar small
(set-face-attribute 'mode-line nil  :height my:font-size)
;; Set the header bar font
(set-face-attribute 'header-line nil  :height my:font-size)
;; Set default window size and position
(setq default-frame-alist
      '((top . 0) (left . 0) ;; position
        (width . 110) (height . 50) ;; size
        ))
;; Enable line numbers on the LHS
;;(global-linum-mode -1)
;; Set the font to size 9 (90/10).
(set-face-attribute 'default nil :height my:font-size)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


;; init.el line #2382
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline theme where the modes are on the right side.
(use-package powerline
  :ensure t
  :config
  (defun powerline-right-theme ()
    "Setup a mode-line with major and minor modes on the right side."
    (interactive)
    (setq-default
     mode-line-format
     '("%e"
       (:eval
        (let* ((active (powerline-selected-window-active))
               (mode-line-buffer-id
                (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
               (mode-line (if active 'mode-line 'mode-line-inactive))
               (face0 (if active 'powerline-active0 'powerline-inactive0))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               ;; The 'r and 'l means "add padding on right/left"
               (lhs (list (powerline-raw
                           (format "W%s|" (winum-get-number-string)) face1)
                          (powerline-raw "L%5l|C%3c|" face1)
                          (powerline-vc face1)
                          (when vc-mode (powerline-raw "|" face1 'l))))
               (center (list
                        (powerline-raw global-mode-string face1 'r)
                        (powerline-buffer-id `(mode-line-buffer-id ,face1))
                        ;; %p is the percentage of the way down the file
                        ;; that we are viewing
                        ;; (powerline-raw "%6p" face1 'r)
                        ))
               (rhs (list ;; Channel tracking if using erc IRC mode
                     (when (and (boundp 'erc-track-minor-mode)
                                erc-track-minor-mode)
                       (powerline-raw erc-modified-channels-object
                                      face2 'l))
                     ;; Show major mode
                     (powerline-raw "|" face1 'l)
                     (powerline-major-mode face2)
                     (powerline-process face2)
                     (powerline-raw "|" face2)
                     (powerline-minor-modes face2)
                     ))
               )
          (concat (powerline-render lhs)
                  (powerline-fill-center
                   face1 (/ (powerline-width center) 2.0))
                  (powerline-render center)
                  (powerline-fill face1 (powerline-width rhs))
                  (powerline-render rhs)))))))
  (powerline-right-theme)
  )

(my:set-custom-faces)


;; init.el line #2300
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable which function mode and set the header line to display both the
;; path and the function we're in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode t)

;; Remove function from mode bar
(setq mode-line-misc-info
      (delete (assoc 'which-func-mode
                     mode-line-misc-info) mode-line-misc-info))


(defmacro with-face (str &rest properties)
  "Used to set the face of STR with PROPERTIES."
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  "."
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]")
         )
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat
           (with-face sl/header
                      ;; :background "red"
                      :foreground "red"
                      :weight 'bold)))
      (concat (if window-system ;; In the terminal the green is hard to read
                  (with-face sl/header
                             ;; :background "green"
                             ;; :foreground "black"
                             :weight 'bold
                             :foreground "#8fb28f"
                             )
                (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "blue"
                           ))
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  "Create the header string and display it."
  ;; The dark blue in the header for which-func is terrible to read.
  ;; However, in the terminal it's quite nice
  (if window-system
      (custom-set-faces
       '(which-func ((t (:foreground "#8fb28f")))))
    (custom-set-faces
     '(which-func ((t (:foreground "blue"))))))
  ;; Set the header line
  (setq header-line-format
        (list "-"
              '(which-func-mode ("" which-func-format))
              '("" ;; invocation-name
                (:eval (if (buffer-file-name)
                           (concat "[" (sl/make-header) "]")
                         "[%b]"))))))
;; Call the header line update
(add-hook 'buffer-list-update-hook 'sl/display-header)

;;; change comment color for better viewing
(set-face-foreground 'font-lock-comment-face "forest green") ; original doom-one is #5B6268
