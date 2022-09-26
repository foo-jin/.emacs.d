;;; base-extensions.el --- General (programming) extensions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'base)

(use-package delight
  :config (delight '((eldoc-mode nil "eldoc")
					 (superword-mode)
					 (auto-revert-mode nil "autorevert"))))


;; https://github.com/abo-abo/avy
(use-package avy
  :delight
  :init
  (defun avy-goto-word-in-line ()
  "Jump to end of whitespace in current line"
  (interactive);; (list (read-char "char: " t)))
  (let (beg end)
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point)))
    (avy-goto-word-0 nil beg end)))
  :bind
  ("M-g M-g" . #'avy-goto-line)
  ("M-g c" . #'avy-goto-word-1)
  ("M-g SPC" . #'avy-goto-word-in-line)
  ("M-g DEL" . #'avy-goto-char-timer))


(use-package ace-window
  :bind ("M-o" . 'ace-window)
  :config
  (custom-set-faces '(aw-leading-char-face ((t (:foreground "cyan" :height 3.0)))))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l)
		aw-scope 'global
		aw-minibuffer-flag t
		aw-dispatch-always t))


(use-package conf-mode
  :bind (:map conf-mode-map ("C-c C-p" . nil)))


;; (use-package imenu-list)
(use-package ledger-mode)


;; https://github.com/hrs/engine-mode
(use-package engine-mode
  :init
  (defengine rust-std
	"file:///home/frank/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=%s")
  (defengine google
	"https://google.com/search?q=%s")
  :bind
  ("M-s e r" . 'engine/search-rust-std)
  ("M-s e g" . 'engine/search-google))


(use-package dashboard
  :delight
  :bind (:map dashboard-mode-map
			  ("n" . 'dashboard-next-line)
			  ("p" . 'dashboard-previous-line))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package recentf
  :delight
  :config
  (setq recentf-save-file (expand-file-name "recentf" temp-dir))
  (recentf-mode 1))


(use-package exec-path-from-shell
  :config
  (when (daemonp)
	(setq exec-path-from-shell-variables
		  '("PATH" "MANPATH" "SSH_AUTH_SOCK" "CARGO_TARGET_DIR" "RIPGREP_CONFIG_PATH" "GNUPGHOME"))
	(exec-path-from-shell-initialize)))


;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :delight
  :bind ("C-<tab>" . 'er/expand-region))


(use-package page-break-lines
  :delight)


(use-package powerline
  :defer
  :delight
  :config
  (powerline-default-theme))


(use-package which-key
  :delight
  :config
  (which-key-mode))


;; (use-package denote)


(use-package erc
  :config
  (setq erc-nick "foo-jin"))


(use-package dirvish
  :init
  (dirvish-override-dired-mode))


(provide 'base-extensions)
;;; base-extensions.el ends here
