(defvar my:dark-mode 'nil)

(if my:dark-mode
    (load-file "~/.emacs.d/theme/dark_theme.el")
  (load-file "~/.emacs.d/theme/light_theme.el"))
