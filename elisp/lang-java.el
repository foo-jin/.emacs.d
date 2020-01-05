;;; lang-java.el --- Java config: lsp-java -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :after lsp
  :hook ((java-mode . lsp)
		 (java-mode . (lambda ()
						(setq indent-tabs-mode nil)))))

;; https://github.com/m0smith/maven-pom-mode
(use-package maven-pom-mode
  :straight (maven-pom-mode :type git
			    :host github
			    :repo "m0smith/maven-pom-mode"))

(provide 'lang-java)
;;; lang-java.el ends here
