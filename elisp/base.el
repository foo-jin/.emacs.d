;;; base.el --- Plain emacs settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
         '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(defconst private-dir  (expand-file-name "private/" user-emacs-directory))
(defconst temp-dir (expand-file-name "emacs/" "~/.cache/"))

;; Emacs customizations
(setq confirm-kill-emacs                  'y-or-n-p
      confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         (expand-file-name "custom.el" private-dir)
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      inhibit-startup-message            t
      fringes-outside-margins            t
      select-enable-clipboard            t
      use-package-always-ensure          t
      inhibit-x-resources                t
      sentence-end-double-space nil
      gc-cons-threshold 100000000)
(blink-cursor-mode)
(load custom-file)

;; indentation
(setq-default indent-tabs-mode t
			  tab-width 4)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; Font face
(set-face-attribute 'default nil
                    :family "Source Code Pro"
					:height 110
                    :width 'normal
                    :weight 'normal)

;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                      t
 bookmark-default-file              (expand-file-name "bookmarks" private-dir))

;; Backups enabled, use nil to disable
(setq
 history-length                     1000
 backup-inhibited                   nil
 make-backup-files                  t
 auto-save-default                  t
 auto-save-list-file-name           (concat temp-dir "/autosave")
 make-backup-files                  t
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(expand-file-name "backup/" temp-dir)))
 auto-save-file-name-transforms    `((".*" ,(expand-file-name "auto-save-list/" temp-dir) t)))

(unless (file-exists-p (expand-file-name "auto-save-list" temp-dir))
		       (make-directory (expand-file-name "auto-save-list/" temp-dir) :parents))

(fset 'yes-or-no-p 'y-or-n-p)

;; Disable toolbar & menubar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(show-paren-mode 1)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'base)
;;; base ends here
