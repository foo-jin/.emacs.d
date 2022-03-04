;;; base-global-keys.el --- Global keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

;; (global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(global-set-key (kbd "C-a") 'move-beginning-of-line-or-indentation)
(global-set-key (kbd "C-e") 'move-end-of-line-or-indentation)

(global-set-key (kbd "M-g w") 'browse-url)
(global-set-key (kbd "M-O") 'switch-to-minibuffer-window)

(global-set-key (kbd "C-c t") 'open-alacritty-in-workdir)

(provide 'base-global-keys)
;;; base-global-keys.el ends here
