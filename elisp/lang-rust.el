;;; lang-rust.el --- Rust config: rust-mode, cargo -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :hook ((rust-mode . lsp)
	 (rust-mode . superword-mode))
  :mode ("\\.rs\\'" . rust-mode)
  :config (setq rust-format-on-save t))

;; toml-mode
;; https://github.com/dryman/toml-mode.el
(use-package toml-mode
  :mode "\\.toml\\'")

(provide 'lang-rust)
;;; lang-rust.el ends here
