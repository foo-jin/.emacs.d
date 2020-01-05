;;; lang-latex.el --- Latex config: auctex -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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


;; pdf-tools
;; https://github.com/politza/pdf-tools
;; M-x pdf-tools-install RET
(use-package pdf-tools)

(provide 'lang-latex)
;;; lang-latex.el ends here
