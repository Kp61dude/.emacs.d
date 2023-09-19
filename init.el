;; initfile --- Summary:
;;; Commentary:
;; Emacs 25.1 and newer tested
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration/Customization:
;; Defines global variables that are later used to customize and set
;; up packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify the ycmd server command and path to the ycmd directory *inside* the
;; cloned ycmd directory. I use a wrapper to add the path to the ycmd clang
;; directories.
(if (file-exists-p "~/.PythonYcmd.sh")
    (defvar my:ycmd-server-command
      '("~/.PythonYcmd.sh" "C:/Users/sapier/AppData/Roaming/YouCompleteMe/third_party/ycmd/ycmd"))
  (defvar my:ycmd-server-command '("python" "C:/Users/sapier/AppData/Roaming/YouCompleteMe/third_party/ycmd/ycmd"))
  )

(defvar my:ycmd-extra-conf-whitelist '("~/.ycm_extra_conf.py"))
(defvar my:ycmd-global-config "~/.ycm_extra_conf.py")
;; In order to get python code completion with ycmd+jedi you must specify
;; the path to the python executable you're using.
(defvar my:ycmd-python-binary-path "C:/Users/sapiens/AppData/Local/Programs/Python/Python311/")

;; Enable ycmd-eldoc support. Eldoc can cause delays when working with
;; template-heavy C++ code.
(defvar my:use-ycmd-eldoc nil)

;; Choose ycmd or lsp for C/C++ completion. lsp or ycmd
(defvar my:cxx-completer "lsp")

;; When t use smart-hungry-delete
;; (https://github.com/hrehfeld/emacs-smart-hungry-delete).
;; When nil use hungry-delete
(defvar my:use-smart-hungry-delete t)

;; Set to t if you want to use ycmd-goto in C/C++/Rust mode
(defvar my:use-ycmd-goto nil)

;; Set to t if you want to use lsp-find-definition in C/C++/Rust mode
(defvar my:use-lsp-goto t)

;; Specify the jupyter executable name, and the start dir of the server
(defvar my:jupyter_location (executable-find "jupyter"))
(defvar my:jupyter_start_dir "c:/Users/EricSapier/AppData/Roaming")

;; In order to get EIN to work with byte-compiling we must explicitly
;; load some of the EIN files. Which files need to be loaded seems to change
;; over time, so to make maintenance easier we provide a variable here that
;; can be updated.
(defvar my:ein-explicit-load-files '(ein ein-notebook ein-jupyter))

;; In order to get lsp-mode to work properly when byte-compiling the init file
;; we must explicitly load its files. This list is produced from all the .el
;; files at:
;;   https://github.com/emacs-lsp/lsp-mode
(defvar my:lsp-explicit-load-files
  '(lsp-completion lsp-diagnostics lsp-headerline lsp-lens lsp-mode
                   lsp-modeline lsp-protocol lsp-sqls lsp))

;; Compilation command for C/C++
(defvar my:compile-command "clang++ -Wall -Wextra -std=c++17 ")
;; Copilation command for C#
(defvar my:compile-command_csharp "dotnet build")
;; Compilation command for smcat
(defvar my:compile-command_smcat "smcat -T pdf -d left-right");;"smcat -T pdf -d left-right"

;; Which theme to use.
;; - spacemacs-dark
;; - sourcerer
;; - doom-* (the doom themes https://github.com/hlissner/emacs-doom-themes)
(defvar my:use-theme 'doom-one-light)

;; Set my:use-dvorak-bindings to t if you use a Dvorak keyboard layout
(defvar my:use-dvorak-bindings nil)

;; Set to t in order to enable using hydra with only x as the activation
;; key. The commands are:
;; - xb starts switch-buffer
;; - xf starts find-file or counsel-find-file
;; - xd starts dired (directory)
;; - xg starts Magit status
;; - xh starts hydra-dispatch
;; - xn starts hydra-move
;; - xx does save-buffer
;; - xk does kill-buffer
;;
;; I've found that this leads to instability and causes Emacs to crash
;; fairly frequently, at least when run in daemon mode.
;;
;; A hydra-dispatch can be accessed using 'C-c h'
(defvar my:use-no-modifier-hydra t)

;; Set my:use-evil-mode to t if you want to use Evil mode
;;
;; Note: Currently there is a warning about evil-want-integration not
;;       being set to nil during compilation that I haven't figured out
;;       how to fix yet.
(defvar my:use-evil-mode nil)

;; Set my:use-prescient to t if you want to use prescient for sorting
;;
;; https://github.com/raxod502/prescient.el
(defvar my:use-prescient t)

;; Set my:byte-compile-init to t if you want to compile the init file.
;; This will improve startup time by ~2-3 times, but makes getting certain
;; packages to load correctly more difficult. Most of the packages work
;; correctly with a byte-compiled init file.
(defvar my:byte-compile-init nil)

;; Force Emacs to try to start a server. On macOS checking if a server is
;; started doesn't always work correctly so this is a workaround for that.
(defvar my:force-server-start nil)

;; Specify the search backend. Must be either:
;; - ivy https://github.com/abo-abo/swiper
;; - selectrum https://github.com/raxod502/selectrum
(defvar my:search-backend "ivy")

;; A list of modes for which to disable whitespace mode
(defvar my:ws-disable-modes '(magit-mode help-mode Buffer-menu-mode dired-mode sql-interactive-mode grep-mode messages-buffer-mode compilation-mode))

;; Modes in which to disable auto-deleting of trailing whitespace
(defvar my:ws-butler-global-exempt-modes
  '(markdown-mode ein:notebook-multilang-mode))

;; TEX is installed in a different location on macOS
;; (when (string-equal system-type "darwin")
  ;; (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
  ;; (setq exec-path (append exec-path '("/Library/TeX/texbin/")))
  ;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

;; Set font size. Font size is set to my:font-size/10
(defvar my:font-size 110)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Disable magically opening remote files during init
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse
           (apply #'nconc
                  ;; Only keep package.el provided loadpaths.
                  (mapcar #'(lambda (path)
                              (if (string-prefix-p package-user-dir-real path)
                                  (list path)
                                nil))
                          load-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 128000000)    ; was 128000000
(add-hook 'after-init-hook
          #'(lambda ()
              ;; restore after startup
              (setq gc-cons-threshold 8000000
                    file-name-handler-alist file-name-handler-alist-old
                    )))

;; Extra plugins and config files are stored here
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start emacs server if not already running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (or my:force-server-start
          (and (fboundp 'server-running-p) (not (server-running-p))))
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use this for benchmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only allow encrypted auth sources
(setq auth-sources '((:source "~/.authinfo.gpg")))

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; Use CUA to delete selections
(setq cua-mode t)
(setq cua-enable-cua-keys nil)
;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)
;; Disable the toolbar at the top since it's useless
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; Auto-wrap at 100 characters
;; (setq-default auto-fill-function 'do-auto-fill)
;; local variable
(setq miFillColumn 150)
(setq-default fill-column miFillColumn)
;; (turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
;; (add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Disable Emacs help on translation, e.g. C-x C-h actually
;; can do things now
;; (define-key key-translation-map [?\C-h] [?\C-?])

;; Global Keyboard Shortcuts
;; Set help to C-?
;; (global-set-key (kbd "C-?") 'help-command)
;; Set mark paragraph to M-?
(global-set-key (kbd "M-?") 'mark-paragraph)
;; Set backspace to C-h
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; Set backspace word to M-h
;; (global-set-key (kbd "M-h") 'backward-kill-word)
;; Use meta+tab word completion
;; (global-set-key (kbd "M-TAB") 'dabbrev-expand)
;; Easy undo key
;; (global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
;; (global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; Indent after a newline, if required by syntax of language
;; (global-set-key (kbd "C-m") 'newline-and-indent)
;; Load the compile command
(global-set-key (kbd "C-c C-c") 'compile)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)

;; Disable the menu bar since we don't use it, especially not in the
;; terminal
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))))

;; Setup use-package
(eval-when-compile
  (require 'use-package))
(use-package bind-key
  :ensure t)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package
  :commands use-package-autoload-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; esup: Emacs StartUp Profiler
;;       - Profile the load time of the Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package esup
  :ensure t
  :init
  (setq esup-child-max-depth 0)
  ;; Use a hook so the message doesn't get clobbered by other messages.
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (message "Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable terminal emacs to copy and paste from system clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: this uses C-c before C-w, M-w, and M-y
;; From: https://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
(defun my-copy-to-xclipboard(arg)
  "Copy the selection ARG to the X11 clipboard."
  (interactive "P")
  (cond
   ((not (use-region-p))
    (message "Nothing to yank to X-clipboard"))
   ((and (not (display-graphic-p))
         (/= 0 (shell-command-on-region
                (region-beginning) (region-end) "xsel -i -b")))
    (message "Error: Is program `xsel' installed?"))
   (t
    (when (display-graphic-p)
      (call-interactively 'clipboard-kill-ring-save))
    (message "Yanked region to X-clipboard")
    (when arg
      (kill-region  (region-beginning) (region-end)))
    (deactivate-mark))))

(defun my-cut-to-xclipboard()
  "Cut the selection to the X11 clipboard."
  (interactive)
  (my-copy-to-xclipboard t))

(defun my-paste-from-xclipboard()
  "Paste the selection from the X11 clipboard."
  (interactive)
  (if (display-graphic-p)
      (clipboard-yank)
    (insert (shell-command-to-string "xsel -o -b"))))

(global-set-key (kbd "C-c C-w") 'my-cut-to-xclipboard)
(global-set-key (kbd "C-c M-w") 'my-copy-to-xclipboard)
;; Use C-c M-y instead of C-c C-y so it works in Python mode too.
(global-set-key (kbd "C-c M-y") 'my-paste-from-xclipboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; async - library for async/thread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s is used by origami, etc and sometimes during Emacs
;; upgrades disappears so we try to install it on its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save ~/.emacs.d/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my:byte-compile-init
  (defun byte-compile-init-files (file)
    "Automatically compile FILE."
    (interactive)
    (save-restriction
      ;; Suppress the warning when you setq an undefined variable.
      (if (>= emacs-major-version 23)
          (setq byte-compile-warnings '(not free-vars obsolete))
        (setq byte-compile-warnings
              '(unresolved
                callargs
                redefine
                obsolete
                noruntime
                cl-warnings
                interactive-only)))
      (byte-compile-file (expand-file-name file))))

  ;; Add a post-save hook that checks if ~/.emacs.d/init.el exists and if the file
  ;; name of the current buffer is ~/.emacs.d/init.el or the symbolically linked
  ;; file.
  (add-hook
   'after-save-hook
   (function
    (lambda ()
      (when (and (string= (file-truename "~/.emacs.d/init.el")
                          (file-truename (buffer-file-name)))
                 (file-exists-p "~/.emacs.d/init.el"))
        (byte-compile-init-files "~/.emacs.d/init.el")))))

  ;; Byte-compile again to ~/.emacs.d/init.elc if it is outdated. We use file-truename
  ;; to follow symbolic links so that ~/.emacs.d/init.ell can be symbolically linked to
  ;; the location where the init.el is stored.
  (when (file-newer-than-file-p
         (file-truename "~/.emacs.d/init.el")
         (file-truename "~/.emacs.d/init.elc"))
    (byte-compile-init-files "~/.emacs.d/init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diminish - Hide the minor modes in the mode line for more room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function diminish "diminish.el"))
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ws-butler-mode
;;
;; Remove trailing white space upon saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (setq ws-butler-global-exempt-modes my:ws-butler-global-exempt-modes)
  (ws-butler-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all-the-icons
;;
;; Used by company-box and some themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select search backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my:use-ivy nil)
(defvar my:use-selectrum nil)
(if (string-match "ivy" my:search-backend)
    (setq my:use-ivy t)
  (if (string-match "selectrum" my:search-backend)
      (setq my:use-selectrum t)
    (warn "my:search-backend must be to 'ivy' or 'selectrum'")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :config
  ;; Define a hydra for movement
  (defhydra hydra-move ()
    "move"
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("F" forward-sexp)
    ("b" backward-char)
    ("B" backward-sexp)
    ("a" mwim-beginning-of-code-or-line) ;; ("a" beginning-of-line) 2021-09-17
    ("e" mwim-end-of-code-or-line) ;; ("e" move-end-of-line) 2021-09-17
    ("u" forward-word)
    ("o" backward-word)
    ("M-f" forward-word)
    ("M-b" backward-word)
    ("h" backward-delete-char)
    ("d" delete-forward-char)
    ("M-h" backward-kill-word)
    ("M-d" kill-word)
    ("s" save-buffer)
    ("/" undo-tree-undo)
    ("x" goto-line)
    ("m" newline-and-indent)
    ("t" transpose-chars)
    ("c" avy-goto-word-1)
    (";" (call-interactively 'commend-or-uncomment-region))

    ;; Selection
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode t)))
    ("M-w" (when (or (bound-and-true-p rectangle-mark-mode)
                     (region-active-p))
             (call-interactively 'kill-ring-save)
             ))
    ("w" (when (or (bound-and-true-p rectangle-mark-mode)
                     (region-active-p))
             (call-interactively 'kill-region)
             ))
    ("SPC" (call-interactively 'set-mark-command))
    ("y" (call-interactively 'yank))
    ("k" (if (bound-and-true-p rectangle-mark-mode)
             (call-interactively 'kill-rectangle)
           (call-interactively 'kill-line)))

    ;; Scrolling
    ("v" scroll-up-command)
    ("M-v" scroll-down-command)
    ("l" recenter-top-bottom)
    ("g" nil "cancel" :color blue)
    )

  (defhydra hydra-dispatch
    (:color blue :hint nil)
    ("f" (call-interactively #'hydra-flyspell-correct/body) "Flyspell")
    ("m" (call-interactively #'hydra-move/body) "Move")
    ("o" (call-interactively #'hydra-origami/body) "Origami")
    ("p" (call-interactively #'hydra-projectile/body) "Projectile")
    ("s" (call-interactively #'hydra-string-inflection/body)
     "String inflection")
    ("q" (call-interactively #'hydra-srefactor/body) "SRefactor")
    ("g" nil "cancel")
    )

  ;; We define a global x-hydra-timer because we need to be able to
  ;; cancel the timer in order to enter projectile from 'xp'
  (defvar x-hydra-timer)
  ;; We save the buffer-undo-list because we want to be able to
  ;; remove inserting the 'x' character from the undo history in the
  ;; case where we entered hydra
  (defvar x-hydra-buffer-undo-list)
  (defun x-hydra-pre ()
    (when (not buffer-read-only)
      (setq x-hydra-buffer-undo-list buffer-undo-list)
      (undo-boundary)
      (insert "x")
      )
    (setq x-hydra-timer (timer-create))
    (timer-set-time x-hydra-timer (timer-relative-time (current-time) 0.5))
    (timer-set-function x-hydra-timer 'hydra-keyboard-quit)
    (timer-activate x-hydra-timer))

  (defhydra x-hydra (:body-pre x-hydra-pre
                               :color blue
                               :hint nil)
    ;; Some Emacs functions like find-file must receive one argument,
    ;; but we want to call it interactively so we need to use
    ;; (call-interactively 'find-file)
    ("b" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'ivy-switch-buffer)))
    ("f" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'find-file)))
    ("d" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'dired)))
    ("g" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'magit-status)))
    ("h" (progn (cancel-timer x-hydra-timer)
                (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively #'hydra-dispatch/body)))
    ("n" (progn (cancel-timer x-hydra-timer)
                (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively #'hydra-move/body)))
    ("o" (progn (cancel-timer x-hydra-timer)
                (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively #'ace-window)))
    ("x" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (save-buffer)))
    ("k" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (kill-buffer)))
    ("3" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'split-window-right)))
    ("2" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'split-window-below)))
    ("1" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'delete-other-windows)))
    ("0" (progn (when (not buffer-read-only)
                  (undo)
                  (setq buffer-undo-list x-hydra-buffer-undo-list))
                (call-interactively 'delete-window)))
    )
  (when my:use-no-modifier-hydra
    (global-set-key "x" #'x-hydra/body))

  ;; Dispatch to hydra
  (global-set-key (kbd "C-c h") #'hydra-dispatch/body)
  )

(use-package use-package-hydra
  :ensure t
  :after hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my:use-ivy
  (use-package ivy
    :ensure t
    :diminish ivy-mode
    :commands (ivy-mode)
    :config
    (when my:byte-compile-init
      (require 'ivy))
    (ivy-mode t)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-wrap t)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    ;; Show #/total when scrolling buffers
    (setq ivy-count-format "%d/%d ")
    )

  ;; Using prescient for sorting results with ivy:
  ;; https://github.com/raxod502/prescient.el
  (when my:use-prescient
    (use-package ivy-prescient
      :ensure t
      :after (counsel)
      :config
      (ivy-prescient-mode t)
      (prescient-persist-mode t)
      )
    )

  (use-package swiper
    :ensure t
    )

  (use-package counsel
    :ensure t
    :bind (("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("<f1> f" . counsel-describe-function)
           ("<f1> v" . counsel-describe-variable)
           ("<f1> l" . counsel-find-library)
           ("C-c i" . counsel-info-lookup-symbol)
           ("C-c u" . counsel-unicode-char)
           ("C-s" . buffer-dependent-swiper)
           ("C-r" . buffer-dependent-swiper)
           ("C-c g" . counsel-git-grep)
           ("C-c j" . counsel-git)
           ("C-c k" . counsel-ag)
           ("C-c r" . counsel-rg)
           ("C-x l" . counsel-locate)
           ("C-c '" . counsel-mark-ring)
           :map minibuffer-local-map
           ("C-r" . counsel-minibuffer-add)
           )
    :config
    (if (executable-find "rg")
        ;; use ripgrep instead of grep because it's way faster
        (setq counsel-grep-base-command
              "rg -i -M 120 --no-heading --line-number --color never %s %s"
              counsel-rg-base-command
              "rg -i -M 120 --no-heading --line-number --color never %s .")
      (warn "\nWARNING: Could not find the ripgrep executable. It "
            "is recommended you install ripgrep."))

    ;; Switch whether we use swiper or counsel-grep depending on the major mode.
    ;; This is because for certain themes font highlighting is very expensive
    ;; in some modes (e.g. C++ mode)
    (defun buffer-dependent-swiper (&optional initial-input)
      (interactive)
      (if (or (not buffer-file-name)
              (ignore-errors
                (file-remote-p (buffer-file-name)))
              (if (or (eq major-mode 'org-mode)
                      (eq major-mode 'c++-mode))
                  (<= (buffer-size) 50000)
                ;; The value 300000 is the default number of characters
                ;; before falling back to counsel-grep from swiper.
                (<= (buffer-size) 300000)))
          (swiper initial-input)
        (progn
          (when (file-writable-p buffer-file-name)
            (save-buffer))
          (counsel-grep initial-input))))
    )

  (use-package counsel-projectile
    :ensure t
    :after (:all counsel projectile)
    :bind (("C-x M-f" . counsel-projectile-find-file-dwim))
    :init
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function counsel-projectile-mode "counsel-projectile.el"))
    :config
    (counsel-projectile-mode))

  ;; Use universal ctags to build the tags database for the project.
  ;; When you first want to build a TAGS database run 'touch TAGS'
  ;; in the root directory of your project.
  (use-package counsel-etags
    :ensure t
    :init
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
      (declare-function counsel-etags-guess-program "counsel-etags.el")
      (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
    :bind (
           ("M-." . counsel-etags-find-tag-at-point)
           ("M-t" . counsel-etags-grep-symbol-at-point))
    :config
    ;; Ignore files above 800kb
    (setq counsel-etags-max-file-size 800)
    ;; Ignore build directories for tagging
    (add-to-list 'counsel-etags-ignore-directories '"build*")
    (add-to-list 'counsel-etags-ignore-directories '".vscode")
    (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    ;; How many seconds to wait before rerunning tags for auto-update
    (setq counsel-etags-update-interval 180)
    ;; Set up auto-update
    (add-hook
     'prog-mode-hook
     (lambda () (add-hook 'after-save-hook
                          (lambda ()
                            (counsel-etags-virtual-update-tags)))))

    ;; The function provided by counsel-etags is broken (at least on Linux)
    ;; and doesn't correctly exclude directories, leading to an excessive
    ;; amount of incorrect tags. The issue seems to be that the trailing '/'
    ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
    ;; in that directory, only files in sub-directories of the dir set to be
    ;; ignore.
    (defun my-scan-dir (src-dir &optional force)
      "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
      (let* ((find-pg (or
                       counsel-etags-find-program
                       (counsel-etags-guess-program "find")))
             (ctags-pg (or
                        counsel-etags-tags-program
                        (format "%s -e -L" (counsel-etags-guess-program
                                            "ctags"))))
             (default-directory src-dir)
             ;; run find&ctags to create TAGS
             (cmd (format
                   "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
                   find-pg
                   (mapconcat
                    (lambda (p)
                      (format "-iwholename \"*%s*\"" p))
                    counsel-etags-ignore-directories " -or ")
                   counsel-etags-max-file-size
                   (mapconcat (lambda (n)
                                (format "-not -name \"%s\"" n))
                              counsel-etags-ignore-filenames " ")
                   ctags-pg))
             (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
             (doit (or force (not (file-exists-p tags-file)))))
        ;; always update cli options
        (when doit
          (message "%s at %s" cmd default-directory)
          (async-shell-command cmd)
          (visit-tags-table tags-file t))))

    (setq counsel-etags-update-tags-backend
          (lambda ()
            (interactive)
            (let* ((tags-file (counsel-etags-locate-tags-file)))
              (when tags-file
                (my-scan-dir (file-name-directory tags-file) t)
                (run-hook-with-args
                 'counsel-etags-after-update-tags-hook tags-file)
                (unless counsel-etags-quiet-when-updating-tags
                  (message "%s is updated!" tags-file))))))
    )

  (use-package flyspell-correct-ivy
    :ensure t
    :after (:all flyspell ivy))

  (use-package lsp-ivy
    :ensure t
    :diminish
    :after (:all lsp-mode ivy))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selectrum config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my:use-selectrum
  (use-package selectrum
    :ensure t
    :config
    (selectrum-mode t)
    (when my:use-prescient
      (use-package selectrum-prescient
        :ensure t
        :config
        (selectrum-prescient-mode t)
        (prescient-persist-mode t)))
    )

  (use-package ctrlf
    :ensure t
    :bind (("C-s" . ctrlf-forward-fuzzy-regexp)
           ("C-r" . ctrlf-backward-fuzzy-regexp)
           ;; ("C-w" . isearch-yank-word-or-char) ; TODO make this work somehow but in ctrlf
           )
    :config
    (ctrlf-mode t)
    ;; It seems sometimes the ctrlf-mode-bindings are reloaded overriding our
    ;; bindings above. To ensure this isn't an issue, we set everything to
    ;; use fuzzy-regexp
    (setq ctrlf-mode-bindings
          '(("C-s"   . ctrlf-forward-fuzzy-regexp)
            ("C-r"   . ctrlf-backward-fuzzy-regexp)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up GNU Global Tags (ggtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (executable-find "global")
  (use-package ggtags
    :ensure t
    :diminish ggtags-mode
    :defer t
    :commands (ggtags-mode)
    :init
    ;; More complicated hook logic so we don't interfere with LSP
    ;; or ycmd-goto
    (when (and (not (and (string-equal my:cxx-completer "lsp")
                         my:use-lsp-goto))
               (not (and (string-equal my:cxx-completer "ycmd")
                         my:use-ycmd-goto)))
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                    (ggtags-mode t)))))
    :config
    ;; Don't try to update GTAGS on each save;
    ;; makes the system sluggish for huge projects.
    (setq ggtags-update-on-save t)
    ;; Don't auto-highlight tag at point.. makes the system really sluggish!
    (setq ggtags-highlight-tag nil)
    ;; Enabling nearness requires global 6.5+
    (setq ggtags-sort-by-nearness t)
    (setq ggtags-navigation-mode-lighter nil)
    (setq ggtags-mode-line-project-name nil)
    (setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :hydra
  (hydra-projectile (:color teal :hint nil)
    ("a"   projectile-ag "ag")
    ("b"   projectile-switch-to-buffer "Switch buffer")
    ("c"   projectile-invalidate-cache "Invalidate cache")
    ("d"   projectile-find-dir "Find dir")
    ("sf"  projectile-find-file "Find file")
    ("ff"  projectile-find-file-dwim "Find file dwim")
    ("fd"  projectile-find-file-in-directory "Find file in dir")
    ("sg"  ggtags-update-tags "Update tags")
    ("i"   projectile-ibuffer "ibuffer")
    ("K"   projectile-kill-buffers "Kill buffers")
    ("sk"  projectile-kill-buffers "Kill buffers")
    ("m"   projectile-multi-occur "Multi-occur")
    ("o"   projectile-multi-occur "Multi-occur")
    ("sp"  projectile-switch-project "Switch project")
    ("r"   projectile-recentf "Recent")
    ("x"   projectile-remove-known-project "Remove project")
    ("X"   projectile-cleanup-known-projects "Cleanup projects")
    ("z"   projectile-cache-current-file "Cache file")
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue)
    ("g"   nil "cancel" :color blue))
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function projectile-mode "projectile.el"))
  :config
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (when my:use-ivy
    (setq projectile-completion-system 'ivy))

  (defun my:projectile-find-file-dwim ()
    "Find the file in the project using DWIM, if not in the use find-file"
    (interactive)
    (if (projectile-project-p)
        (call-interactively 'projectile-find-file-dwim)
      (call-interactively 'projectile-find-file)))
  (when my:use-selectrum
    (global-set-key (kbd "C-x M-f") 'my:projectile-find-file-dwim)
    (setq projectile-completion-system 'default))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package winum (actively maintained replacement of window-numbering)
;; installed from package list. Allows switching between buffers using
;; meta-(# key)
(use-package winum
  :ensure t
  :init
  (defvar winum-keymap
        (let ((map (make-sparse-keymap)))
          ;; (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  :config
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function winum-mode "winum.el"))
  (setq winum-scope 'frame-local)
  (winum-mode t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
  :ensure t
  :defer 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ripgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (executable-find "rg")
  (use-package rg
    :ensure t
    :config
    (rg-enable-default-bindings)
    (when my:use-selectrum
      (global-set-key (kbd "C-c g") 'rg-project)
      (global-set-key (kbd "C-c r") 'rg-project))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit server to allow editing of things in Chrome with Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package edit-server
  :ensure t
  :config
  (progn
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function edit-server-start "edit-server-start.el"))
    (when (daemonp) (edit-server-start))
    (add-hook 'edit-server-start-hook
              (lambda ()
                (when (string-match "github.com" (buffer-name))
                  (markdown-mode)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :hydra (hydra-origami (:color pink :columns 4)
                        "Origami Folds"
                        ("t" origami-recursively-toggle-node "Toggle")
                        ("s" origami-show-only-node "Single")
                        ("r" origami-redo "Redo")
                        ("u" origami-undo "Undo")
                        ("o" origami-open-all-nodes "Open")
                        ("c" origami-close-all-nodes "Close")
                        ("n" origami-next-fold "Next")
                        ("p" origami-previous-fold "Previous")
                        ("q" nil "Quit" :color blue)
                        ("g" nil "cancel" :color blue))
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :commands which-key-mode
  :defer 2
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function which-key-mode "which-key.el"))
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: always fast jump to char inside the current view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)
         ("C-M-c" . avy-goto-char-2)
         ("C-'" . avy-goto-char))
         ;; ([remap goto-line] . avy-goto-line))
  ;; Set keys for Dvorak mode instead of qwerty
  :init
  (when my:use-dvorak-bindings
    (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
                        ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
                        ?p ?y ?f ?g ?c ?r ?l
                        ?P ?Y ?F ?G ?C ?R ?L))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zzz-to-char: replaces the built-in zap-to-char with avy-like
;;              replacement options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zzz-to-char
  :ensure t
  :bind ("M-z" . zzz-up-to-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use undo-tree to navigate undo history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :defer 1
  :config
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-undo-tree-mode "undo-tree.el"))
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual-regexp-steroids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-c v r" . vr/query-replace)
         ("M-%" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)
         ("C-M-s" . vr/isearch-forward)
         ("C-M-r" . vr/isearch-backward))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RealGud - https://github.com/realgud/realgud
;; A rewrite of GUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package realgud
  :ensure t
  :after (c-mode-common python-mode csharp-mode)
  :init
  (setenv "TERM" "dumb")
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "python")
  :hook
  ((python-mode . (lambda ()
                    (setq tab-width 4)))
   (python-mode . lsp-deferred))
  :config
  (setq-default python-indent 4)
  (setq-default python-indent-offset 4)
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (setq tab-width 4)))
  )

(setq-default pdb-command-name "python -m pdb")
(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :after python
  :config
  (elpy-enable)
  )

(use-package yapfify
  :ensure t
  :hook (python-mode . yapf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-region))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-rename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(if (not (file-exists-p "~/.emacs.d/plugins/emacs-clang-rename.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/nilsdeppe/emacs-clang-rename/master/emacs-clang-rename.el"
     "~/.emacs.d/plugins/emacs-clang-rename.el"))
(when (file-exists-p "~/.emacs.d/plugins/emacs-clang-rename.el")
    (use-package emacs-clang-rename
      :bind (("C-c c p" . emacs-clang-rename-at-point)
             ("C-c c q" . emacs-clang-rename-qualified-name)
             ("C-c c a" . emacs-clang-rename-qualified-name-print))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern C++ code highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode)
  :init
  (eval-when-compile
      ;; Silence missing function warnings
    (declare-function modern-c++-font-lock-global-mode
                      "modern-cpp-font-lock.el"))
  :config
  (modern-c++-font-lock-global-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (setq compile-command my:compile-command)
  (custom-set-variables '(c-noise-macro-names '("constexpr")))
  (use-package google-c-style
    :ensure t
    :config
    ;; This prevents the extra two spaces in a namespace that Emacs
    ;; otherwise wants to put... Gawd!
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    ;; Autoindent using google style guide
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
    )
  )

;; Change tab key behavior to insert spaces instead
(setq-default indent-tabs-mode nil)

;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 2)
;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 4)

;; We want to be able to see if there is a tab character vs a space.
;; global-whitespace-mode allows us to do just that.
;; Set whitespace mode to only show tabs, not newlines/spaces.
(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :diminish whitespace-mode
  :init
  (eval-when-compile
      ;; Silence missing function warnings
      (declare-function global-whitespace-mode "whitespace.el"))
  :config
  (setq whitespace-style '(face lines-tail trailing tabs tab-mark))
  (setq-default whitespace-line-column miFillColumn)
  (setq whitespace-line 'nil)
  )

;; Turn on whitespace mode globally except in magit-mode
(define-global-minor-mode my-global-whitespace-mode whitespace-mode
  (lambda ()
    (let* ((allow-ws-mode t))
      (progn
        (dolist (element my:ws-disable-modes)
          (when (derived-mode-p element)
            (setq allow-ws-mode nil)
            )
          )
        (when allow-ws-mode
          (whitespace-mode t))))
    ))
(my-global-whitespace-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: ycmd (YouCompleteMeDaemon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up YouCompleteMe for emacs:
;; https://github.com/Valloric/ycmd
;; https://github.com/abingham/emacs-ycmd
(defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))
;; (when (not my:python-location)
;;     (message
;;      "Could not start YouCompleteMeDaemon because the python executable could
;; not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
;; appropriately in ~/init.el.\n" (nth 0 my:ycmd-server-command)))
;; (when (not (file-directory-p (nth 1 my:ycmd-server-command)))
;;     (message "Could not YouCompleteMeDaemon because the specified directory does
;; not exist.\nSpecified directory is: '%s'
;; Please set my:ycmd-server-command appropriately in ~/init.el.\n"
;;              (nth 1 my:ycmd-server-command)))
(when (and my:python-location
           (file-directory-p (nth 1 my:ycmd-server-command))
           (string-equal my:cxx-completer "ycmd"))
    (use-package ycmd
      :ensure t
      :diminish ycmd-mode
      :hook (c-mode-common . ycmd-mode)
      :init
      ;; (eval-when-compile
      ;;   ;; Silence missing function warnings
      ;;   (declare-function global-ycmd-mode "ycmd.el"))
      ;; (add-hook 'after-init-hook #'ycmd-mode)

      ;; Only override the CTags shortcut if my:use-ycmd-goto is t
      (when my:use-ycmd-goto
        (add-hook 'c-mode-common-hook
                  '(lambda ()
                     (local-set-key (kbd "M-.") 'ycmd-goto))))
      :config
      (progn
        (set-variable 'ycmd-server-command my:ycmd-server-command)
        (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
        (set-variable 'ycmd-global-config (file-truename my:ycmd-global-config))
        (set-variable 'ycmd-python-binary-path my:ycmd-python-binary-path)
        (setq ycmd-force-semantic-completion t)
        ;; Use "C-c y" instead of "C-c Y" for the prefix
        (define-key ycmd-mode-map ycmd-keymap-prefix nil)
        (setq ycmd-keymap-prefix (kbd "C-c y"))
        ;; Switch around some of the ycmd keybindings to make them easier to
        ;; use. Mainly, fewer capital letters.
        (setq ycmd-command-map
              (let ((map (make-sparse-keymap)))
                (define-key map "b" 'ycmd-parse-buffer)
                (define-key map "o" 'ycmd-open)
                (define-key map "c" 'ycmd-close)
                (define-key map "." 'ycmd-goto)
                (define-key map "gi" 'ycmd-goto-include)
                (define-key map "gd" 'ycmd-goto-definition)
                (define-key map "gD" 'ycmd-goto-declaration)
                (define-key map "gm" 'ycmd-goto-implementation)
                (define-key map "gp" 'ycmd-goto-imprecise)
                (define-key map "gr" 'ycmd-goto-references)
                (define-key map "gt" 'ycmd-goto-type)
                (define-key map "s" 'ycmd-toggle-force-semantic-completion)
                (define-key map "v" 'ycmd-show-debug-info)
                (define-key map "V" 'ycmd-version)
                (define-key map "d" 'ycmd-show-documentation)
                (define-key map "C" 'ycmd-clear-compilation-flag-cache)
                (define-key map "O" 'ycmd-restart-semantic-server)
                (define-key map "t" 'ycmd-get-type)
                (define-key map "p" 'ycmd-get-parent)
                (define-key map "f" 'ycmd-fixit)
                (define-key map "r" 'ycmd-refactor-rename)
                (define-key map "x" 'ycmd-completer)
                map))

        (define-key ycmd-mode-map ycmd-keymap-prefix ycmd-command-map)

        (use-package company-ycmd
          :ensure t
          :init
          (eval-when-compile
            ;; Silence missing function warnings
            (declare-function company-ycmd-setup "company-ycmd.el"))
          :config
          (company-ycmd-setup)
          )

        (use-package flycheck-ycmd
          :ensure t
          :init
          (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
          (add-hook 'rust-mode-hook 'flycheck-ycmd-setup)
          )

        ;; Add displaying the function arguments in mini buffer using El Doc
        (when my:use-ycmd-eldoc
          (require 'ycmd-eldoc)
          (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))
        )
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: lsp (language server protocol mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A code completion, syntax checker, etc. engine that uses the LSP to
;; talk to completion servers.
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (;; Python on Linux/mac OS is pyls (python language server)
         (python-mode . lsp)
         ;; Rust RLS (Rust Language Server) https://github.com/rust-lang/rls
         (rust-mode . lsp)
         ;; Bash uses bash-language-server
         ;; https://github.com/mads-hartmann/bash-language-server
         (shell-mode . lsp)
         ;; sql uses what? 06-08-2021
         (sql-interactive-mode . lsp)
         )
  :init
  ;; Disable yasnippet. We re-enable when yasnippet is loaded.
  (defvar lsp-enable-snippet nil)
  (use-package lsp-ui
    :ensure t
    :after lsp-mode
    :hook (lsp-mode . lsp-ui-mode)
    :config
    ;; Use find references and definitions key bindings instead of CTags.
    (defun set-local-keybinds-lsp-ui ()
      "Sets keybindings for lsp mode"
      (interactive)
      (local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
      (local-set-key (kbd "M-?") 'lsp-ui-peek-find-references)
      )
    (add-hook 'c-mode-common-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'python-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'rust-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'shell-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'sql-interactive-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'csharp-mode 'set-local-keybinds-lsp-ui)
    )

  ;; Use as C++ completer if desired. We use the clangd backend.
  (when (string-equal my:cxx-completer "lsp")
    (add-hook 'c-mode-common-hook #'lsp)
    ;;
    (add-hook 'lsp-mode-hook
              '(lambda ()
                 (when my:use-lsp-goto
                   (local-set-key (kbd "M-.") 'lsp-find-definition)))))

  :config
  (when my:byte-compile-init
      (dolist (lsp-file my:lsp-explicit-load-files)
        (require lsp-file)))
  ;; Set GC threshold to 25MB since LSP mode is very memory hungry and
  ;; produces a lot of garbage
  (setq gc-cons-threshold 25000000)

  ;; Increase the amount of data which Emacs reads from the process. The emacs
  ;; default is too low 4k considering that the some of the language server
  ;; responses are in 800k - 3M range. Set to 1MB
  (setq read-process-output-max (* 1024 1024))

  ;; Extra flags passed to clangd. See 'clangd --help' for info
  (defvar lsp-clients-clangd-args '("--clang-tidy"
                                    "--fallback-style=google"
                                    "-j=4"
                                    "--enable-config"
                                    "--suggest-missing-includes"
                                    "--pch-storage=memory"))
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-before-save-edits nil)
  ;; Use flycheck instead of flymake
  (setq lsp-prefer-flymake nil)
  ;; Change prefix to C-c y (like ycmd)
  (define-key lsp-mode-map (kbd "C-c y") lsp-command-map)

  ;; Set keybindings
  (local-set-key (kbd "C-c y n") 'lsp-rename)
  (local-set-key (kbd "C-c y o") 'lsp-restart-workspace)
  (local-set-key (kbd "C-c y c") 'lsp-disconnect)
  (local-set-key (kbd "C-c y f") 'lsp-format-region)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode . global-company-mode)
  :commands (company-mode company-indent-or-complete-common)
  :init
  (setq company-minimum-prefix-length 1 ; was 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)

        ;; These auto-complete the current selection when
        ;; `company-auto-complete-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-complete nil
        company-auto-complete-chars nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil

        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  :config
  (defvar my:company-explicit-load-files '(company company-capf))
  (when my:byte-compile-init
    (dolist (company-file my:company-explicit-load-files)
      (require company-file)))
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  ;; remove backends for packages that are dead
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  )

;; Use prescient for sorting results with company:
;; https://github.com/raxod502/prescient.el
(when my:use-prescient
  (use-package company-prescient
    :ensure t
    :after company
    :config
    (company-prescient-mode t)
    (prescient-persist-mode t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add icons to code completion when using the GUI client.
;; https://github.com/sebastiencs/company-box/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck with LSP mode
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )
(use-package flycheck-pyflakes
  :ensure t
  :after python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-inflection
;; used for switching between different cases, eg CamelCase,
;; lowerCamelCase, snake_case, and SCREAMING_SNAKE_CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package string-inflection
  :ensure t
  :defer t
  :bind (("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase))
  :hydra (hydra-string-inflection
          ()
          "Inflection"
          ("i" string-inflection-cycle "Cycle")
          ("l" string-inflection-lower-camelcase "camelCase")
          ("c" string-inflection-camelcase "CamelCase")
          ("s" string-inflection-underscore "Underscore")
          ("u" string-inflection-upcase "Upcase")
          ("q" nil "Quit" :color blue)
          ("g" nil "Quit" :color blue))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors  - https://github.com/magnars/multiple-cursors.el
;; Allows you to have multiple cursors on different lines so you can
;; easily edit multiple lines at once.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c m d" . mc/mark-all-dwim)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m e" . mc/edit-lines)
         ;;; my own keybinds
         ;; ("C->" . mc/mark-next-like-this)
         ;; ("C-<" . mc/mark-previous-like-this)
         ;; ("C-c C-<" . mc/mark-all-like-this)
         )
  :init
  (defun ar/set-mc/insert-numbers-starting-value ()
    "set starting value for inserting numbers using multiple cursors."
    (interactive)
    (set-variable 'mc/insert-numbers-default
                  (read-number "starting value: ")))
  :config
  (defalias 'mc/mark-all-lines-in-region 'mc/edit-lines)
  ;; mc-friendly packages.
  (use-package phi-search :ensure t)
  (use-package phi-rectangle :ensure t)
  (use-package phi-search-mc :ensure t
    :config
    (phi-search-mc/setup-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done 'time
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" .
                                (:foreground "blue" :weight bold))))
(use-package writegood-mode
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function writegood-mode "writegood-mode.el"))
  (add-hook 'org-mode-hook #'writegood-mode)
  )

(use-package org
  :mode  ("\\.org\\'" . org-mode)
  :init
  (defun sa-ignore-headline (contents backend info)
    "Ignore headlines with tag `ignoreheading'."
    (when (and (org-export-derived-backend-p backend 'latex 'html 'ascii)
               (string-match "\\`.*ignoreheading.*\n"
                             (downcase contents)))
      (replace-match "" nil nil contents)))
  :config
  (defun org-show-current-heading-tidily ()
    "Show next entry, keeping other entries closed."
    (interactive)
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (show-children))
      (outline-back-to-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)))
  ;; backend aware export preprocess hook
  ;; https://github.com/suvayu/.emacs.d/blob/master/org-mode-config.el#L234

  ;; (add-to-list 'org-export-before-parsing-hook #'sa-ignore-headline) ;; moved to hooks

  ;; (setq org-ellipsis "⤵")
  (setq org-ellipsis "↓")
  ;; (setq org-ellipsis " v")
  (setq org-list-allow-alphabetical t)
  (add-to-list 'org-modules 'org-tempo)
  (use-package org-superstar
    :ensure t)
  (use-package org-contrib
    :ensure t
    ;;:after org
    :config
    (require 'ox-extra)
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))
  (setq org-adapt-indentation t)
  :hook
  (org-mode . org-superstar-mode)
  (org-mode . turn-on-auto-fill)
  (org-mode . (lambda () (setq fill-column 100)))
  (org-mode . yas-minor-mode)
  (org-mode . display-fill-column-indicator-mode)
  (org-mode . whitespace-mode)
  (org-mode .(lambda () (setq tab-width 4)))
  :bind (("\C-cl" . org-store-link)
         :map org-mode-map
         ([remap org-cycle-agenda-files] . avy-goto-char)
         ("\C-ca" . org-agenda)
         ("C-c C-x l" . org-todo-list)
         ("C-=" . er/mark-word)
         ([remap org-ctrl-c-tab] . org-show-current-heading-tidily)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vlf - handle open very large files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ein - ipython notebooks in gui emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only launch if the executable exists.
(when (and my:jupyter_location my:jupyter_start_dir)
  ;; Skewer mode is used by ein for running javascript in the notebook
  (use-package skewer-mode
    :ensure t)
  (use-package ein
    :ensure t
    :requires skewer-mode
    :commands (ein:login ein:jupyter-server-start)
    :config
    (when my:byte-compile-init
      (dolist (ein-file my:ein-explicit-load-files)
        (require ein-file)))
    ;; when editing the emacs.el file, we do not want to start a new
    ;; Jupyter server each time we save, so we only start a new Jupyter
    ;; server if there currently isn't one running.
    (defvar my-found-ein-server nil)
    (dolist (my-current-process (process-list))
      (when (string-match "EIN: Jupyter*" (process-name my-current-process))
        (setq my-found-ein-server t))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load hungry Delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set hungry delete:
(if my:use-smart-hungry-delete
    (use-package smart-hungry-delete
      :ensure t
      :bind (("<backspace>" . smart-hungry-delete-backward-char)
             ;; ("C-h" . smart-hungry-delete-backward-char)
             ("C-d" . smart-hungry-delete-forward-char))
      :init
      (eval-when-compile
        ;; Silence missing function warnings
        (declare-function
         smart-hungry-delete-add-default-hooks "smart-hungry-delete.el"))
      :config
      ;; (require 'smart-hungry-delete)
      (smart-hungry-delete-add-default-hooks)
      )
  (use-package hungry-delete
    :ensure t
    :diminish hungry-delete-mode
    :init
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function global-hungry-delete-mode "hungry-delete.el"))
    :config
    (global-hungry-delete-mode t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting in CUDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell Mode for Spelling Corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (org-mode . flyspell-mode))
  :hydra
  (hydra-flyspell-correct
   (:color blue)
   "Flyspell Correct"
   ("a" flyspell-correct-auto-mode "Auto Mode" :color red)
   ("b" flyspell-buffer "Check buffer")
   ("r" flyspell-region "Check region")
   ("n" flyspell-check-next-highlighted-word "Next Word")
   ("p" flyspell-check-previous-highlighted-word "Previous Word")
   ("w" (call-interactively 'flyspell-correct-at-point) "Word at Point")
   )
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function flyspell-goto-next-error "flyspell.el")
    (declare-function flyspell-mode "flyspell.el")
    (declare-function flyspell-prog-mode "flyspell.el"))
  (setq flyspell-issue-welcome-flag nil)
  (use-package flyspell-correct
    :ensure t
    :diminish flyspell-correct-mode
    :after flyspell)
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))

  (global-set-key (kbd "<f7>") 'flyspell-buffer)
  (global-set-key (kbd "<f8>") 'flyspell-correct-previous)
  (global-set-key (kbd "<f9>") 'flyspell-correct-next)
  (define-key flyspell-mode-map (kbd "M-0") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil) ; turn this off as it runs into iedit default keybinding
  ;; brought over from my own setup
  (setq ispell-hunspell-dict-paths-alist
        '(("en_US" "~/hunspell/dictionary/en_US.aff")))
  (setq ispell-local-dictionary "en_US")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :after (:any ivy selectrum)
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :init
  (use-package dash
    :ensure t)
  (use-package forge
    :ensure t
    :after magit)
  :config
  (when my:use-ivy
    (setq magit-completing-read-function 'ivy-completing-read))
  :hook (git-commit-mode . yas-minor-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-hl
;;
;; git-gutter is no longer maintained so use diff-hl instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         ;; (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         )
  :commands (diff-hl-mode)
  :init
  (use-package diff-hl-amend
    :commands (diff-hl-amend-mode)
    )
  (use-package diff-hl-dired
    :commands (diff-hl-dired-mode)
    )
  (use-package diff-hl-flydiff
    :commands (diff-hl-flydiff-mode)
    )
  (use-package diff-hl-margin
    :commands (diff-hl-margin-mode)
    )
  :config
  ;; Use purple to show diffs
  (custom-set-faces
     '(diff-hl-change
       ((t (:background "#5f00af" :foreground "#5f00af")))))
  (diff-hl-margin-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-timemachine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-timemachine
  :ensure t
  :bind (("M-g M-t" . git-timemachine))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gitignore-mode: highlighting in gitignore files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/gitignore-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/magit/git-modes/master/gitignore-mode.el"
     "~/.emacs.d/plugins/gitignore-mode.el"))
(when (file-exists-p "~/.emacs.d/plugins/gitignore-mode.el")
  (use-package gitignore-mode
  :diminish gitignore-mode
  :mode ("\\.gitignore\\'"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (add-to-list 'company-backends 'company-cmake)))
  :config
  (use-package cmake-font-lock
    :ensure t
    :defer t
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                  1 font-lock-warning-face t)))))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bazel-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(if (not (file-exists-p "~/.emacs.d/plugins/bazel-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/codesuki/bazel-mode/master/bazel-mode.el"
     "~/.emacs.d/plugins/bazel-mode.el"))
(if (file-exists-p "~/.emacs.d/plugins/bazel-mode.el")
    (use-package bazel-mode
      :mode ("BUILD" "\\.bazel\\'" "\\.bzl'" "WORKSPACE\\'")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(if (not (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/protocolbuffers/protobuf/master/editors/protobuf-mode.el"
     "~/.emacs.d/plugins/protobuf-mode.el"))
(if (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el")
    (use-package protobuf-mode
      :mode ("\\.proto\\'")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.imp\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'")
  :init
  (when my:byte-compile-init
    (require 'rust-mode))
  (use-package flycheck-rust
    :ensure t
    :after rust-mode)

  :config
  (defun my:rust-mode-hook()
    (set (make-local-variable 'compile-command) "cargo run")
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function flycheck-rust-setup "flycheck-rust.el"))
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    )
  (add-hook 'rust-mode-hook 'my:rust-mode-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Dockerfile mode
;; 1. Download file from GitHub
;; 2. Load mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/dockerfile-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/spotify/dockerfile-mode/master/dockerfile-mode.el"
     "~/.emacs.d/plugins/dockerfile-mode.el"))
(use-package dockerfile-mode
  :mode ("Dockerfile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :defer 30
  :diminish yas-minor-mode
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; Add snippet support to lsp mode
  (setq lsp-enable-snippet t)
  ;; (yas-global-mode t)
  )
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))
;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
;;
;; The above seems to not be an issue with LSP, but it is still nice to be
;; able to only call up the snippets.
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet company)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
  :mode ("\\.s\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (define-key markdown-mode-map (kbd "M-p") nil)
  (define-key markdown-mode-map (kbd "M-n") nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lua-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode))
  :config
  (add-hook 'lua-mode-hook #'company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; When we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :config
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-source-correlate-start-server t)
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (message "Windows does not have a PDF viewer set for auctex")))
   ((string-equal system-type "darwin") ; Mac OS X
    (setq-default
     TeX-view-program-list
     '(("Skim"
        "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
       )
     TeX-view-program-selection '((output-pdf "Skim"))))
   ((string-equal system-type "gnu/linux") ; linux
    (setq-default TeX-view-program-list
                  '(("Evince" "evince --page-index=%(outpage) %o"))
                  TeX-view-program-selection '((output-pdf "Evince")))))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq-default reftex-plug-into-AUCTeX t)
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-mode: Emacs Vi Layer to use Vi/Vim keybindings in Emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when my:use-evil-mode
  (use-package evil
    :ensure t
    :diminish undo-tree-mode
    :init
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function evil-mode "evil.el"))
    (setq evil-want-integration nil)
    :config
    (evil-mode t)
    (use-package powerline
      :ensure t
      :config
      (powerline-center-evil-theme))

    ;; There is a warning about evil-want-integration not being set to
    ;; nil during compilation that I haven't figured out how to fix.
    (use-package evil-collection
      :after evil
      :ensure t
      :init
      (when my:byte-compile-init
        ;; We need company-tng when byte-compiling, otherwise don't load it
        ;; since it'll slow down startup times.
        (require 'company-tng))
      (eval-when-compile
        ;; Silence missing function warnings
        (declare-function evil-collection-init "evil-collection.el"))
      :config
      (evil-collection-init)
      )

    (when my:use-dvorak-bindings
      (use-package evil-dvorak
        :ensure t
        :diminish evil-dvorak-mode
        :init
        (eval-when-compile
          ;; Silence missing function warnings
          (declare-function global-evil-dvorak-mode "evil-dvorak.el"))
        :config (global-evil-dvorak-mode t)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load c++-mode when opening charm++ interface files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ci\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load SpEC files in specinput mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (file-exists-p (expand-file-name "~/.emacs.d/plugins/specinput-mode.el"))
    (use-package specinput-mode
      :mode ("\\.input\\'" "\\.output\\'")
      ))

(add-to-list 'auto-mode-alist '("\\.def\\'" . bash-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . bash-mode))
(add-to-list 'auto-mode-alist '("\\.toplevel\\'" . perl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personally added
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package graphviz-dot-mode
  :ensure t)




(when (file-exists-p "~/.emacs.d/plugins/smcat-mode.el")
  (message "SMCAT found dude!")
  (use-package smcat-mode
    :hook
    (smcat-mode . (lambda () (setq tab-width 4)))
    :init
    (defun cur-file()
      (file-name-nondirectory (buffer-file-name (current-buffer))))
    :config
    (setq tab-width 4)
    (setq indent-tabs-mode nil)

    ;; *compilation local to smcat*
    ;;(setq compile-command my:compile-command_smcat)
    ;;(define-key smcat-mode-map (kbd "C-c C-c") 'compile)
    ;; https://stackoverflow.com/questions/12756531/using-the-current-buffers-file-name-in-m-x-compile
    (add-hook 'smcat-mode-hook (lambda ()
                                 (set (make-local-variable 'compile-command)
                                      (concat "smcat -T png " (cur-file)))))
  )
)


;; Extend eshell: Completion
;; https://timmydouglas.com/2020/12/18/eshell-complete.html
(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "Main Porcelain Commands")
    (let (commands)
      (while (re-search-forward
              "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
              nil t)
        (push (match-string 1) commands)
        (when (match-string 2)
          (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
                             (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))


;;; count-words-region
(use-package simple
  :bind ("C-M-=" . count-words-region))


;;; lin mode
;; https://raw.githubusercontent.com/protesilaos/lin/main/lin.el
;; https://protesilaos.com/codelog/2022-01-08-emacs-face-remap-add-relative/
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/lin.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/protesilaos/lin/main/lin.el"
     "~/.emacs.d/plugins/lin.el"))
(when (file-exists-p "~/.emacs.d/plugins/lin.el")
  (use-package lin))


;; Clip current file to clipboard
;; https://stackoverflow.com/questions/18812938/copy-full-file-path-into-copy-paste-clipboard
(defun clip-file ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (gui-select-text filename))))


;; mode for powershell
(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode)
  )


;; mode for C#
(use-package csharp-mode
  :ensure t
  :mode ("\\.cs\\'")
  :config
  (setq compile-command my:compile-command_csharp)
  (setq tab-width 4)
  (setq-default csharp-indent 4)
  (setq-default csharp-indent-offset 4)
  (add-hook 'csharp-mode-hook
          (lambda ()
            (setq tab-width 4)))
  (define-key csharp-mode-map (kbd "C-c") nil) ; turn off comment-region
  (define-key csharp-mode-map (kbd "C-c C-c") 'compile)
  )


;;; change comment color for better viewing
(set-face-foreground 'font-lock-comment-face "forest green") ; original doom-one is #5B6268
;;; change string color
;; (set-face-foreground 'font-lock-string-face "red") ; not used


;;; iec61131-mode
  ;; manually edit iec61131-mode.el:
  ;; (define-derived-mode
  ;; iec61131-mode prog-mode ;fundamental-mode
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/iec61131-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/wadoon/st-mode/master/iec61131-mode.el"
     "~/.emacs.d/plugins/iec61131-mode.el"))
(when (file-exists-p "~/.emacs.d/plugins/iec61131-mode.el")
  (use-package iec61131-mode
    :mode ("\\.st\\'"
           "\\.TcGVL\\'"
           "\\.TcPOU\\'"
           "\\.TcDUT\\'")
    :hook
    (iec61131-mode .  (lambda () (setq tab-width 4)))
    (iec61131-mode . my-comment-remap-mode)

    :config
    (defvar-local my-comment-remap-cookie nil
      "Cookie of the last 'face-remap-add-relative'.")

    ;; open large .tsproj files in text-mode
    (add-to-list 'auto-mode-alist '("\\.tsproj\\'" . text-mode))

    (define-minor-mode my-comment-remap-mode
      "Remap the face of comments."
      :local t
      :init-value nil
      (if my-comment-remap-mode
          (setq my-comment-remap-cookie
                (face-remap-add-relative 'font-lock-comment-face 'font-lock-string-face))
        (face-remap-remove-relative my-comment-remap-cookie)))
    )
  )


;;; srefactor
(use-package srefactor
  :ensure t
  :hydra
  (hydra-srefactor (:color teal :hint nil)
                   ("o" srefactor-lisp-one-line "srefactor-lisp-one-line")
                   ("m" srefactor-lisp-format-sexp "srefactor-lisp-format-sexp")
                   ("d" srefactor-lisp-format-defun "srefactor-lisp-format-defun")
                   ("b" srefactor-lisp-format-buffer "srefactor-lisp-format-buffer")
                   ("e" srefactor-refactor-at-point "srefactor-refactor-at-point"))

  ;; how to set your own map?
  ;; (setq ycmd-command-map
  ;;             (let ((map (make-sparse-keymap)))
  ;;               (define-key map "b" 'ycmd-parse-buffer)
  ;;               (define-key map "o" 'ycmd-open)
  ;;               (define-key map "c" 'ycmd-close)
  ;;               (define-key map "." 'ycmd-goto)
  ;;               (define-key map "gi" 'ycmd-goto-include)
  ;;               (define-key map "gd" 'ycmd-goto-definition)
  ;;               (define-key map "gD" 'ycmd-goto-declaration)
  ;;               (define-key map "gm" 'ycmd-goto-implementation)
  ;;               (define-key map "gp" 'ycmd-goto-imprecise)
  ;;               (define-key map "gr" 'ycmd-goto-references)
  ;;               (define-key map "gt" 'ycmd-goto-type)
  ;;               (define-key map "s" 'ycmd-toggle-force-semantic-completion)
  ;;               (define-key map "v" 'ycmd-show-debug-info)
  ;;               (define-key map "V" 'ycmd-version)
  ;;               (define-key map "d" 'ycmd-show-documentation)
  ;;               (define-key map "C" 'ycmd-clear-compilation-flag-cache)
  ;;               (define-key map "O" 'ycmd-restart-semantic-server)
  ;;               (define-key map "t" 'ycmd-get-type)
  ;;               (define-key map "p" 'ycmd-get-parent)
  ;;               (define-key map "f" 'ycmd-fixit)
  ;;               (define-key map "r" 'ycmd-refactor-rename)
  ;;               (define-key map "x" 'ycmd-completer)
  ;;               map))

  :config
  ;; (setq srefactor-ui-menu-show-help nil) ;; hide the help message in the menu
  (semantic-mode 1) ;; -> this is optional for Lisp
  (define-key srefactor-ui-menu-mode-map (kbd "C-c q") 'srefactor-command-map)
)

;;; smartparens
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

;;; keep init.el clean and move custom-set-variables to ~/custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;;; Set up .bat file mode
(use-package bat-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.bat\\'" . bat-mode))
  )

;;; Set up openwith
(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("rtf"))
               "winword"                ; microsoft word
               '(file))
         (list (openwith-make-extension-regexp
                '("mp4"))
               "wmplayer"               ;windows media player
               '(file))
         (list (openwith-make-extension-regexp
                '("sln"))
               "TcXaeShell"             ; Twincat shell
               '(file))
         (list (openwith-make-extension-regexp
                '("sldasm"))
               "sldworks"               ; Solidworks assembly
               '(file))
         (list (openwith-make-extension-regexp
                '("sldprt"))
               "sldworks"               ; Solidworks part
               '(file))
         (list (openwith-make-extension-regexp
                '("dxf"))
               "sldworks"               ; Solidworks DXF
               '(file))
         ))
  (openwith-mode 1)
  )


;; Lisp specific defuns
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
;; bind command to C-x C-9
(global-set-key (kbd "C-x C-9") 'eval-and-replace)

;;; set a register to this file for easy access
(set-register ?i '(file . "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load SQL (builtin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package sql
;;   ;; :defer 30
;;   :init
;;   (defalias 'sql-get-login 'ignore) ; suppress login as I only use one DB.
;;   (add-to-list 'exec-path "C:/Program Files/MySQL/MySQL Server 5.7/bin/")
;;   (setq sql-mysql-program "C:/Program Files/MySQL/MySQL Server 5.7/bin/MySQL.exe"
;;         sql-mysql-options '("-C" "-f" "-t" "-n")
;;         sql-user "root"
;;         sql-password "root"
;;         sql-database "systemscelldb"
;;         sql-server "127.0.0.1")
;;   (setq lsp-sqls-server "c:/Users/sapier/go/bin/sqls.exe")
;;   (setq lsp-sqls-connections
;;         '(((driver . "mysql") (dataSourceName . "root:root@tcp(127.0.0.1:3306)/systemscelldb"))))
;;   (setq lsp-sqls-timeout 0.0)

;;   (use-package sqlup-mode
;;     :ensure t
;;     :after(sql)
;;     :config
;;     (add-to-list 'sqlup-blacklist "name")
;;     (add-to-list 'sqlup-blacklist "operation"))

;;   (use-package sql-indent
;;     :ensure t)

;;   :hook ((sql-interactive-mode . sqlup-mode)
;;          (sql-interactive-mode . sqlind-minor-mode)
;;          (sql-mode . sqlup-mode)
;;          (sql-mode . lsp)
;;          (sql-interactive-mode . yas-minor-mode))
;;   :bind(:map sql-interactive-mode-map
;;              ("C-M-a" . sql-beginning-of-statement)))

;;; turn off line wrap globally
(setq-default truncate-lines 1)

;;; ace-mode setup
(use-package ace-window
:ensure t
:bind(("M-o" . ace-window)
      ("C-M-p" . ace-swap-window)
      ))

;;; scrolling
(setq scroll-step 1
      scroll-conservatively 10000)
;; mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressice-speed nil)
(setq mouse-wheel-follow-mouse 't)

;;; activate expand-region.el
;; find more info here: https://github.com/magnars/expand-region.el
;; and watch emacsrocks episode 9 for a tutorial
(use-package expand-region
  :ensure t
  :bind(("C-=" . er/expand-region)))

;;; kill buffer immediately after key combo
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; use bash in org-mode
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)))

;;; spelling stuff
(global-set-key (kbd "M-0") 'ispell-word)

;;; iedit mode default binding is "C-;"
(use-package iedit
  :defer 30
  :ensure t
  :bind (([remap iedit-show/hide-context-lines] . nil))
  )

;;; semantic
(use-package semantic
  :defer 6
  :bind("C-c j" . semantic-ia-fast-jump))

(use-package drag-stuff
  :ensure t
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

;;; dired
(use-package dired
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  (setq ls-lisp-dirs-first t)
  (setq dired-listing-switches "-lav")
  (setq dired-dwim-target t)
  :hook
  (dired-mode . dired-hide-details-mode)
  :bind (([remap list-directory] . dired)
         ([remap dired] . dired-other-window)
         ([remap dired-prev-subdir] . ace-swap-window))
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file) ; prevents opening multiple dired
              ("^" . (lambda () (interactive) (find-alternate-file "..")))))

(use-package dired-x
  :bind(("C-x C-j" . dired-jump)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xenodium
;;; github link: https://github.com/xenodium/dotsies/tree/main/emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from fe-navigation.el
;; Smarter move to beginning/end of line.
(use-package mwim
  :ensure t
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning-of-code-or-line)
              ("C-e" . mwim-end-of-code-or-line)
              :map org-mode-map
              ("C-a" . mwim-beginning-of-code-or-line)
              ("C-e" . mwim-end-of-code-or-line)
              ;; :map nxml-mode-map
              ;; ("C-a" . mwim-beginning-of-code-or-line)
              ;; ("C-e" . mwim-end-of-code-or-line) ; this is broken for some reason
              ))

;; anchored-transpose (tweaked to my own liking and not
;; copied directly from xenodium)
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/anchored-transpose.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/emacsmirror/anchored-transpose/master/anchored-transpose.el"
     "~/.emacs.d/plugins/anchored-transpose.el"))
(when (file-exists-p "~/.emacs.d/plugins/anchored-transpose.el")
  (use-package anchored-transpose
    :commands anchored-transpose
    :init
    ;; which used to be transpose-words
    (global-unset-key (kbd "M-t"))
    :bind
    (("M-t r" . anchored-transpose)
     ("M-t l" . transpose-lines)
     ("M-t w" . transpose-words)))
  )


(provide 'init.el)
;;; init.el ends here
