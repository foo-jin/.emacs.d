;;; base-selection.el --- Config for incremental narrowing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; github.com/raxod502/selectrum
(use-package selectrum
  :config
  (selectrum-mode +1))

;; github.com/raxod502/prescient.el
(use-package prescient
  :config
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1))

(use-package company-prescient
  :config
  (company-prescient-mode))

;; github.com/oantolin/orderless
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (savehist-mode)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; (use-package ivy
;;   :delight
;;   :hook (after-init . ivy-mode)
;;   :bind
;;   ("C-s" . swiper)
;;   ("C-r" . swiper)
;;   ("C-x C-r" . ivy-resume)
;;   :config
;;   (setq ivy-use-virtual-buffers nil
;; 		ivy-use-selectable-prompt t))

;; (use-package lsp-ivy
;;   :bind (:map lsp-mode-map ("C-c s" . lsp-ivy-workspace-symbol))
;;   :commands lsp-ivy-workspace-symbol)

;; github.com/minad/consult#configuration
(use-package consult
  :bind
  ("C-x b" . #'consult-buffer)
  ("M-i" . #'consult-imenu)
  ("M-y" . #'consult-yank-pop)
  ("M-g M-g" . #'consult-line)
  ("M-g o" . #'consult-outline)
  ("M-g f" . #'consult-flycheck)
  ("M-s r" . #'consult-ripgrep)
  ("M-s g" . #'consult-git-grep)
  ("M-s s" . #'consult-lsp-symbols)
  ("M-s M-s" . #'consult-lsp-file-symbols)
  ("C-c p" . #'consult-projectile)
  :config
  (setq consult-narrow-key "<")
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key '(:debounce 0.5 any)))

(use-package consult-flycheck)

(use-package consult-projectile
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))


(provide 'base-selection)
;;; base-selection.el ends here
