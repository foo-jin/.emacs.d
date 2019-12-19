;;; base-theme.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)
(use-package gruvbox-theme
  :defer t
  :init
  (load-theme 'gruvbox-dark-medium))

(provide 'base-theme)
;;; base-theme.el ends here
