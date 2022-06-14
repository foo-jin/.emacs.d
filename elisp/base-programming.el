;;; programming.el --- General config for programming -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'base)

(use-package smartparens
  :delight
  :hook ((prog-mode . smartparens-mode)
		 (conf-mode . smartparens-mode))
  :config
  (require 'smartparens-config))


(use-package diff-hl
  :diminish
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
		 (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode t)
  (global-diff-hl-mode))

(use-package highlight-indent-guides
  :diminish
  :hook ((prog-mode . hightlight-indent-guides-mode))
  :config (setq highlight-indent-guides-method 'bitmap))

;; (use-package company
  ;; :delight
  ;; :hook ((prog-mode . company-mode))
  ;; :bind (:map company-active-map ("<tab>" . #'company-complete-common-or-cycle))
  ;; :config
  ;; (setq company-minimum-prefix-length 2
		;; company-idle-delay 0.75
		;; company-tooltip-align-annotations t))

;; (use-package company-box
  ;; :delight
  ;; :hook (company-mode . company-box-mode))


;; emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :diminish
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
			  ("M-RET" . #'lsp-execute-code-action)
			  ("C-c r" . #'lsp-rename))
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (delight '((lsp-lens-mode nil "Lens")))
  (setq lsp-eldoc-enable-hover t
		lsp-signature-auto-activate t
		lsp-signature-render-documentation t
		lsp-signature-doc-lines 5
	    lsp-rust-analyzer-cargo-watch-command "clippy"
		lsp-keep-workspace-alive nil
		lsp-auto-execute-action nil))

(use-package lsp-ui
  :bind (:map lsp-mode-map ("C-h ." . #'lsp-ui-doc-glance))
  :config
  (setq lsp-ui-doc-enable t
		lsp-ui-doc-location 'at-point))

(use-package consult-lsp
  :after (consult lsp)
  :bind ("M-s s" . consult-lsp-symbols)
        ("M-s M-s" . consult-lsp-file-symbols))

(use-package eldoc
  :pin gnu
  :diminish
  :bind ("C-c d" . #'eldoc))

;; github.com/joaotavora/eglot
;; (use-package eglot
;;   ;; :hook ((rustic-mode . eglot-ensure)) ;; rustic mode takes care of this
;;   :bind (:map eglot-mode-map ("C-c r" . #'eglot-rename)
;; 			                 ("M-RET" . #'eglot-code-actions))
;;   :custom
;;   (eglot-autoshutdown t))

;; (use-package consult-eglot
;;   :bind (:map eglot-mode-map ("M-g s" . #'consult-lsp-symbols)))


;; emacs-tree-sitter.github.io
(use-package tree-sitter
  :delight)
(use-package tree-sitter-langs)


(use-package flymake
  :hook ((prog-mode . flymake-mode)
		 (markdown-mode . flymake-mode))
  :bind (:map flymake-mode-map ("M-n" . #'flymake-goto-next-error)
			                   ("M-p" . #'flymake-goto-prev-error))
  :custom
  (flymake-mode-line-format '(" " flymake-mode-line-counters)))


;; https://github.com/mohkale/flymake-collection
(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

;; flycheck
;; (use-package flycheck
;;   :delight
;;   :config
;;   (setq lsp-prefer-flymake nil)

;;   (define-key flycheck-mode-map flycheck-keymap-prefix nil)
;;   (setq flycheck-keymap-prefix (kbd "C-c f"))
;;   (define-key flycheck-mode-map flycheck-keymap-prefix
;; 			  flycheck-command-map)
;;   (global-flycheck-mode))

;; github.com/magit/magit
(use-package magit
  :bind ("C-c g" . magit-file-dispatch)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; github.com/magit/forge
(use-package forge
  :after magit
  :config
  (setq forge-database-file (expand-file-name "forge-database.sqlite" private-dir)
		forge-owned-accounts '((foo-jin))))


(use-package project
  :pin gnu
  :bind (("C-c k" . #'project-kill-buffers)
		 ("C-c m" . #'project-compile)
		 ("C-c f" . #'project-find-file)
		 ("C-c p" . #'project-switch-project)
		 ("C-c b" . #'project-switch-to-buffer))
  :config
  (setq project-list-file (expand-file-name "project-bookmarks.eld" private-dir)
		project-switch-commands '((project-find-file "Find file")
								  (magit-status "Magit" ?g)
								  (consult-ripgrep "rg" ?r))
		compilation-always-kill t))

;; joaotavora.github.io/yasnippet/index.html
(use-package yasnippet
  :bind (:map yas-minor-mode-map
			  (("C-c y" . 'yas-expand)))
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all))

;; https://github.com/Wilfred/deadgrep
(use-package deadgrep)

;; (use-package ediff
;;   :config
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq-default ediff-highlight-all-diffs 'nil)
;;   (setq ediff-diff-options "-w"))


(provide 'base-programming)
;;; base-programming.el ends here
