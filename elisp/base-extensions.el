;;; base-extensions.el --- General (programming) extensions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package delight
  :config (delight '((eldoc-mode nil "eldoc"))))

;; https://github.com/abo-abo/avy
(use-package avy
  :delight
  :bind
  ("C-;" . avy-goto-char))

(use-package counsel
  :delight
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop)
  :config (counsel-mode))

(use-package ivy
  :delight
  :hook (after-init . ivy-mode)
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  (:map read-expression-map ("C-r" . counsel-expression-history))
  :config
  (setq ivy-use-virtual-buffers nil))


(use-package company
  :delight
  :hook ((prog-mode . company-mode))
  :config
  (setq company-idle-delay 0.1
	company-tooltip-align-annotations t))

(use-package company-lsp
  :delight
  :commands company-lsp
  :after (lsp-mode company)
  :config (push 'company-lsp company-backends))


;; lsp-mode
(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp
  :config
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-width 50
	lsp-ui-doc-max-height 20))


;; flycheck
(use-package flycheck
  :delight
  :config (setq lsp-prefer-flymake nil)
  :init (global-flycheck-mode))


(use-package dashboard
  :delight
  :config
  (dashboard-setup-startup-hook))


(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))


(use-package expand-region
  :delight
  :bind ("C-=" . er/expand-region))


(use-package counsel-projectile
  :delight
  :bind-keymap ("C-c p" . projectile-command-map))


(use-package hlinum
  :delight
  :config
  (hlinum-activate))

(use-package linum
  :delight
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))


(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)


(use-package page-break-lines
  :delight)


(use-package powerline
  :delight
  :config
  (powerline-default-theme))


(use-package projectile
  :delight '(:eval (format " [%s]" (projectile-project-name)))
  :config
  (setq projectile-known-projects-file
	(expand-file-name "projectile-bookmarks.eld" temp-dir))
  (setq projectile-completion-system 'ivy)
  (projectile-mode))


(use-package rainbow-delimiters
  :delight
  :hook ((prog-mode . rainbow-delimiters-mode)))


(use-package recentf
  :delight
  :config
  (setq recentf-save-file (expand-file-name "recentf" temp-dir))
  (recentf-mode 1))


(use-package smartparens
  :delight)


(use-package undo-tree
  :delight
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))


(use-package which-key
  :delight
  :config
  (which-key-mode))


(use-package wgrep
    :delight)

(use-package yasnippet
  :hook ((rust-mode . yas-minor-mode))
  :bind (:map yas-minor-mode-map ("C-c y" . #'yas-expand)
	      ("<tab>" . nil)
	      ("TAB" . nil))
  :config (yas-reload-all))

(provide 'base-extensions)
;;; base-extensions.el ends here
