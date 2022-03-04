;;; lang-modes.el --- Config for specific programming major-modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/brotzeit/rustic
(use-package rustic
  :hook ((rustic-mode . yas-minor-mode)
		 (rustic-mode . lsp-deferred)
		 (rustic-mode . tree-sitter-hl-mode)
		 (before-save . (lambda () (when (eq 'rustic-mode major-mode)
									 (lsp-format-buffer)))))
  :bind (:map rustic-mode-map
			  ("C-c C-c c a" . rustic-cargo-add)
			  ("C-c C-c c r" . rustic-cargo-rm)
			  ("C-c TAB" . rustic-popup))
  :config
  (setq compilation-scroll-output t))

;; https://github.com/dryman/toml-mode.el
(use-package toml-mode
  :hook ((toml-mode . prog-mode))
  :mode "\\.toml\\'")

;; https://github.com/juergenhoetzel/pkgbuild-mode
(use-package pkgbuild-mode)
(use-package yaml-mode
  :delight)


;; Auctex
(use-package auctex
  :hook ((LaTeX-mode . auto-fill-mode)
		 (LaTeX-mode . flyspell-mode)
		 (LaTeX-mode . LaTeX-math-mode))
  :init
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-auto-save t
		TeX-parse-self t
		Tex-PDF-mode t
		TeX-view-program-selection '((output-pdf "PDF Tools"))
		TeX-source-correlate-start-server t)
  (setq-default TeX-master nil))

;; https://github.com/politza/pdf-tools
;; M-x pdf-tools-install RET
(use-package pdf-tools
  :config
  (pdf-tools-install))


;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :defer t
  :after lsp
  :hook ((java-mode . lsp)
		 (java-mode . (lambda ()
						(setq indent-tabs-mode nil)))))


;; https://github.com/juergenhoetzel/pkgbuild-mode
(use-package pkgbuild-mode)
(use-package yaml-mode
  :delight)


(provide 'lang-modes)
;;; lang-modes.el ends here
