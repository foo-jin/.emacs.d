;;; base-theme.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)
(use-package gruvbox-theme
  :defer t)

(use-package twilight-bright-theme
  :defer t)

(use-package doom-themes
  :init
  (load-theme 'doom-homage-white)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config))

(require 'generic-x)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(flycheck-error-list-highlight ((t (:background "light goldenrod"))))
;;  '(highlight ((t (:background "light goldenrod" :foreground "#282828"))))
;;  '(isearch ((t (:background "green yellow" :foreground "black"))))
;;  '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:underline (:color "DarkOrange" :style wave)))) t)
;;  '(lsp-ui-doc-background ((t (:background "cornsilk"))))
;;  '(lsp-ui-sideline-code-action ((t (:foreground "dodger blue")))))

(provide 'base-theme)
;;; base-theme.el ends here
