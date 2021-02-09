;;; base-theme.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)
(use-package gruvbox-theme
  :defer t)

(use-package twilight-bright-theme
  :defer t)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config)
  (load-theme 'doom-homage-white))


(provide 'base-theme)
;;; base-theme.el ends here
