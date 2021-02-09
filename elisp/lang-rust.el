;;; lang-rust.el --- Rust config: rust-mode, cargo -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; rustic-mode
(use-package rustic
  :bind (:map rustic-mode-map
			  ("C-c C-c a" . rustic-cargo-add)
			  ("C-c C-c r" . rustic-cargo-rm))
  :config
  (setq rustic-format-on-save t
		rustic-lsp-format t))

;; toml-mode
;; https://github.com/dryman/toml-mode.el
(use-package toml-mode
  :hook ((toml-mode . prog-mode))
  :mode "\\.toml\\'")

(provide 'lang-rust)
;;; lang-rust.el ends here
