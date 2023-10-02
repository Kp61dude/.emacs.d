(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-noise-macro-names '("constexpr"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(company-graphviz-dot python-mode graphviz-dot-mode phi-search-mc phi-rectangle phi-search smcat-mode zzz-to-char yasnippet-snippets yapfify yaml-mode ws-butler writegood-mode winum which-key web-mode vlf visual-regexp-steroids use-package-hydra undo-tree string-inflection srefactor spacemacs-theme smartparens smart-hungry-delete rust-mode rg realgud rainbow-delimiters powershell powerline platformio-mode origami org-superstar org-onenote org-contrib openwith mwim multiple-cursors modern-cpp-font-lock lua-mode lsp-ui lsp-ivy json-mode ivy-prescient iedit hydra google-c-style git-timemachine ggtags gcode-mode forge flyspell-correct-ivy flycheck-rust flycheck-pyflakes expand-region evil esup elpy edit-server drag-stuff doom-themes diminish diff-hl cuda-mode csharp-mode counsel-projectile counsel-etags company-prescient company-box cmake-mode clang-format ccls benchmark-init beacon auto-package-update auctex arduino-mode all-the-icons ace-window))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "white"))))
 '(avy-lead-face ((t (:foreground "white" :background "#9e51b5" :bold t :underline t))))
 '(company-tooltip ((t (:background nil))))
 '(company-tooltip-selection ((t (:background nil :underline t))))
 '(diff-hl-change ((t (:background "#5f00af" :foreground "#5f00af"))))
 '(highlight ((t (:underline t))))
 '(ivy-current-match ((t (:background nil :underline t))))
 '(powerline-active1 ((t (:background "white" :foreground "#9e51b5" :bold t :underline t))))
 '(powerline-active2 ((t (:background "white" :foreground "#9e51b5" :bold t :underline t))))
 '(which-func ((t (:foreground "blue")))))


(message "Loaded: my custom.el")
