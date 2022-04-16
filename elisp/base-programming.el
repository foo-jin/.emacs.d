;;; programming.el --- General config for programming -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'base)

(use-package smartparens
  :hook ((prog-mode . smartparens-mode))
  :config
  (require 'smartparens-config))


(use-package company
  :delight
  :hook ((prog-mode . company-mode))
  :bind (:map company-active-map ("<tab>" . #'company-complete-common-or-cycle))
  :config
  (setq company-minimum-prefix-length 2
		company-idle-delay 0.1
		company-tooltip-align-annotations t))

(use-package company-box
  :delight
  :hook (company-mode . company-box-mode))


;; emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map ("M-RET" . lsp-execute-code-action))
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-signature-auto-activate t
		lsp-rust-analyzer-server-display-inlay-hints nil
	    lsp-rust-analyzer-cargo-watch-command "clippy"))

(use-package lsp-ui
  :bind (:map lsp-mode-map ("C-c q" . lsp-ui-doc-show))
  :config (setq lsp-ui-doc-enable nil
				lsp-ui-sideline-show-hover t
				lsp-ui-peek-always-show t
				lsp-ui-doc-position 'at-point))

;; emacs-tree-sitter.github.io
(use-package tree-sitter)
(use-package tree-sitter-langs)


;; flycheck
(use-package flycheck
  :delight
  ;; :init (setq flycheck-keymap-prefix (kbd "C-c f"))
  :config (setq lsp-prefer-flymake nil)
  :init (global-flycheck-mode))


(use-package projectile
  :delight '(:eval (format " [%s]" (projectile-project-name)))
  :bind-keymap ("C-x p" . projectile-command-map)
  :init
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" temp-dir)
		projectile-switch-project-action #'magit-status)
  (projectile-mode))


;; github.com/magit/magit
(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; attempt to fix magit symbol issue
(use-package project)

;; github.com/magit/forge
(use-package forge
  :after magit
  :config
  (setq forge-database-file (expand-file-name "forge-database.sqlite" private-dir)
		forge-owned-accounts '((foo-jin))))


(use-package yasnippet
  :config (yas-reload-all))

;; https://github.com/Wilfred/deadgrep
(use-package deadgrep)

;; (use-package ediff
;;   :config
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq-default ediff-highlight-all-diffs 'nil)
;;   (setq ediff-diff-options "-w"))


(provide 'base-programming)
;;; base-programming.el ends here
