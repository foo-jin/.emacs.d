;;; base-extensions.el --- General (programming) extensions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'base)

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

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	aw-scope 'frame))


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
  :commands lsp
  :config
  (setq lsp-enable-snippet nil
	lsp-rust-server 'rust-analyzer))

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
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))


(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))


(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))


(use-package expand-region
  :delight
  :bind ("C-<tab>" . er/expand-region))


(use-package counsel-projectile
  :delight
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (counsel-projectile-mode))


(use-package hlinum
  :delight
  :config
  (hlinum-activate))

(use-package linum
  :delight
  :hook ((prog-mode . linum-mode))
  :config
  (setq linum-format " %3d "))


(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read
	magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package pass)


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
  (setq projectile-completion-system 'ivy
	projectile-switch-project-action 'magit-status)
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
  :hook ((prog-mode . smartparens-mode))
  :config
  (require 'smartparens-config))


(use-package vterm)


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
