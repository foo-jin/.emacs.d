;;; base-global-keys.el --- Global keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

;; (global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c o") 'browse-url)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)


(provide 'base-global-keys)
;;; base-global-keys.el ends here
