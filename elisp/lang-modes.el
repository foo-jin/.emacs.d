;;; lang-modes.el --- Config for specific programming major-modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/brotzeit/rustic
(use-package rustic
  :hook ((rustic-mode . yas-minor-mode)
		 ;; (rustic-mode . eglot-ensure)
		 ;; (before-save . (lambda () (when (eq 'rustic-mode major-mode)
									 ;; (lsp-format-buffer))))
		 (rustic-mode . tree-sitter-hl-mode))
  :bind
  (:map rustic-mode-map
		("C-c <tab>" . rustic-popup)
		("C-c C-<tab>" . rustic-popup)
		("C-c C-p" . nil))
  :config
  (setq rustic-lsp-client 'lsp-mode
		rustic-format-trigger 'on-save
		rustic-format-display-method 'ignore
		compilation-scroll-output t))


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


(provide 'lang-modes)
;;; lang-modes.el ends here
