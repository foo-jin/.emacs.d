;;; init.el --- generated from `init.org' - do not edit by hand! -*- lexical-binding: t -*-
;;; Commentary:
;;;     `org-tangle'-d from `init.org'. Edits should go there, and not here.
;;; Code:
(setq debug-on-error t)

(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

(defconst private-dir (expand-file-name "private/" user-emacs-directory))
(defconst temp-dir (expand-file-name "emacs/" "~/.cache")) ;; expand ~

(setq-default initial-scratch-message ""
			  tab-width 4)
  (fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer t
	  save-interprogram-paste-before-kill t
	  mouse-yank-at-point t
	  visible-bell nil
	  ring-bell-function 'ignore
	  custom-file (expand-file-name "custom.el" private-dir)
	  cursor-in-non-selected-windows nil
	  highlight-nonselected-windows nil
	  inhibit-startup-message t
	  fringes-outside-margins t
	  select-enable-clipboard t
	  inhibit-x-resources t
	  sentence-end-double-space nil
	  ;; Bookmarks
	  bookmark-save-flag t
	  bookmark-default-file (expand-file-name "bookmarks" private-dir)
	  ;; Backups
	  backup-inhibited nil
	  make-backup-files t
	  auto-save-default t
	  auto-save-list-file-name (concat temp-dir "/autosave")
	  create-lockfiles nil
	  backup-directory-alist `((".*" . ,(expand-file-name "backup/" temp-dir)))
	  auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" temp-dir) t)))
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; ensure "auto-save-list" folder exists
(unless (file-exists-p (expand-file-name "auto-save-list" temp-dir))
  (make-directory (expand-file-name "auto-save-list/" temp-dir) :parents))
(set-face-attribute 'default nil
					:family "Source Code Pro"
					:height 110
					:width 'normal
					:weight 'normal)

(load custom-file)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'save-place-mode)
  (setq save-place-file (expand-file-name "places" temp-dir))
  (save-place-mode +1)
  (setq-default save-place t))
(blink-cursor-mode)
(show-paren-mode 1)

(straight-use-package 'org)
(use-package org
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  ;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle :append :local)))
  (setq org-capture-templates '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
								 "* TODO %?\nAdded: %U\n %i"))
		org-use-fast-todo-selection t
		org-startup-indented nil
		org-ellipsis "  "
		org-pretty-entities t
		org-pretty-entities-include-sub-superscripts nil
		org-hide-emphasis-markers t
		org-special-ctrl-a/e t
		org-hide-leading-stars nil
		org-highlight-latex-and-related '(latex))

  ;; org-babel stuff
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
	 (shell . t)
	 (calc . t)
	 (gnuplot . t)))

  (setq org-src-fontify-natively t
		org-src-window-setup 'current-window)
  :bind
  ("C-c C-l" . 'org-store-link)
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture))

(use-package ob-async)
(use-package ob-sagemath
  :config
  (setq org-babel-default-header-args:sage '((:session . t)
											 (:results . "output"))))

;; this breaks stuff!
;; (use-package org-appear
  ;; :hook (org-mode . 'org-appear-mode))
;;; init.el ends here

(use-package doom-themes
  :init
  (setq custom-safe-themes t)
  (load-theme 'doom-homage-white)
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t)
  (doom-themes-org-config))

(use-package highlight-numbers
  :hook (prog-mode conf-mode))

(use-package hl-line
  :straight nil
  :hook (prog-mode text-mode))

(use-package powerline
  :disabled
  :delight
  :config
  (powerline-revert))

(use-package delight
	:config   ;;    MODE     /VALUE/  /FILE/
	(delight '((eldoc-mode       nil "eldoc")
			   (superword-mode)
			   (auto-revert-mode nil "autorevert")
			   (buffer-face-mode nil "face-remap"))))

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package undo-tree
  :delight
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo" temp-dir)))))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package exec-path-from-shell
  :config
  (when (daemonp)
	(setq exec-path-from-shell-variables
		  '("PATH" "MANPATH" "SSH_AUTH_SOCK" "CARGO_TARGET_DIR" "RIPGREP_CONFIG_PATH" "GNUPGHOME")))
  (exec-path-from-shell-initialize))

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

;; (global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(global-set-key (kbd "C-a") 'move-beginning-of-line-or-indentation)
(global-set-key (kbd "C-e") 'move-end-of-line-or-indentation)

(global-set-key (kbd "M-g w") 'browse-url)
(global-set-key (kbd "M-O") 'switch-to-minibuffer-window)
(global-set-key (kbd "C-c t") 'open-alacritty-in-workdir)
(global-set-key (kbd "C-.") 'completion-at-point)
(global-set-key [remap dabbrev-expand] 'hippie-expand) ;; M-/
(global-set-key (kbd "C-x |") 'toggle-window-split)

(use-package avy
  :delight
  :init
  (defun avy-goto-word-in-line ()
  "Jump to word in current line"
  (interactive)
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

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (savehist-mode)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)))

(use-package prescient
  :config
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches
		prescient-save-file (expand-file-name "prescient-save.el" private-dir))
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1))

(use-package consult
  :ensure
  :bind (("C-x b" . 'consult-buffer)
		 ("M-i" . 'consult-imenu)
		 ("M-y" . 'consult-yank-pop)
		 ("M-g o" . 'consult-outline)
		 ("M-g f" . 'consult-flymake)
		 ("M-g m" . 'consult-mark)
		 ("M-g M" . 'consult-global-mark)
		 ("M-s r" . 'consult-ripgrep)
		 ("M-s g" . 'consult-git-grep)
		 ("M-s f" . 'consult-fd)
		 ("M-s F" . 'consult-locate)
		 ("M-s l" . 'consult-line)
		 ("M-s L" . 'consult-line-multi)
		 :map isearch-mode-map
		 ("C-p" . 'consult-isearch-history))
  :config
  (when (executable-find "plocate")
	(setq consult-locate-args "plocate --ignore-case --existing --regexp"))
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-compile-error
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key '(:debounce 3 any)))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . 'consult-dir)
		 :map selectrum-minibuffer-map
		 ("C-x C-d" . 'consult-dir)
		 ("C-x C-j" . 'consult-dir-jump-file)))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map ("M-A" . 'marginalia-cycle))
  :config
  ;; save after cycling
  (advice-add #'marginalia-cycle :after
			  (lambda ()
				(when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  (advice-add #'marginalia-cycle :after
			  (lambda ()
				(let ((inhibit-message t))
				  (customize-save-variable 'marginalia-annotator-registry
										   marginalia-annotator-registry)))))

(use-package embark
  :disabled
  :bind (("C-." . 'embark-act)
		 ("C-," . 'embark-dwim)
		 ("C-h B" . 'embark-bindings)
		 :map embark-file-map
		 ("s" . 'sudo-find-file))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :disabled
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . 'consult-preview-at-point-mode))

(use-package dashboard
  :delight
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook)
  :bind (:map dashboard-mode-map
			  ("n" . 'dashboard-next-line)
			  ("p" . 'dashboard-previous-line)))

(use-package recentf
  :delight
  :config
  (setq recentf-save-file (expand-file-name "recentf" temp-dir))
  (recentf-mode 1))

(use-package engine-mode
  :init
  (defengine rust-std "https://doc.rust-lang.org/std/?search=%s")
  ;; Unfortunately `rust-local` fails to go directly to the search page,
  ;; if not there would be no need for the online documentation here.
  (defengine rust-local
	(concat
	 "file://"
	 (expand-file-name
	  "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=%s")))
  (defengine google
	"https://google.com/search?q=%s")
  :bind
  ("M-s e r" . 'engine/search-rust-std)
  ("M-s e g" . 'engine/search-google))

;; M-x pdf-tools-install RET
(use-package pdf-tools
  :config
  (pdf-tools-install))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode 'display-line-numbers-mode)

(use-package smartparens
  :delight
  :hook (prog-mode conf-mode)
  :config
  (require 'smartparens-config))

(use-package corfu
  :init
  (global-corfu-mode))

(use-package tree-sitter
  :delight)
(use-package tree-sitter-langs)

(use-package flymake
  :hook (prog-mode)
  :bind (:map flymake-mode-map
			  ("M-n" . #'flymake-goto-next-error)
			  ("M-p" . #'flymake-goto-prev-error))
  :custom
  (flymake-mode-line-format '(" " flymake-mode-line-counters)))


(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

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

(use-package deadgrep)

(use-package yasnippet
  :bind (:map yas-minor-mode-map
			  (("C-c y" . 'yas-expand)))
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all))

(use-package just-mode)

(use-package pkgbuild-mode)

(use-package yaml-mode
  :delight
  :mode "\.ya?ml\'")

(use-package json-mode
  :mode "\\.json\\'")
(use-package flymake-json
  :hook (json-mode . 'flymake-json-load))

(use-package markdown-mode
  :disabled
  :hook (markdown-mode . 'flyspell-mode)
  :init
  (setq-default markdown-hide-markup t))

(use-package fish-mode)

(use-package gnuplot)

;; github.com/magit/magit
(use-package magit
  :bind ("C-c g" . 'magit-file-dispatch)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; github.com/magit/forge
(use-package forge
  :after magit
  :config
  (setq forge-database-file (expand-file-name "forge-database.sqlite" private-dir)
		forge-owned-accounts '((foo-jin))))

;; github.com/magit/orgit
(use-package orgit)

(use-package diff-hl
  :diminish
  :hook ((magit-pre-refresh magit-post-refresh) . 'diff-hl-magit-pre-refresh)
  :config
  (diff-hl-flydiff-mode t)
  (global-diff-hl-mode))

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
		lsp-rust-analyzer-rustfmt-extra-args "+nightly"
		lsp-keep-workspace-alive nil
		lsp-auto-execute-action nil))
  ;; (lsp-rust-analyzer-inlay-hints-mode))
;; (lsp-register-custom-settings '(("some.setting.asdf" nil nil)))

(use-package lsp-ui
  :bind (:map lsp-mode-map ("C-c d" . #'lsp-ui-doc-glance))
  :config
  (setq lsp-ui-doc-enable nil
		lsp-ui-doc-location 'at-point))

(use-package consult-lsp
  :after lsp
  :bind (("M-s s" . 'consult-lsp-symbols)
		 ("M-s M-s" . 'consult-lsp-file-symbols)))

(use-package eldoc
  :pin gnu
  :diminish
  :bind ("C-c d" . #'eldoc))

;; github.com/joaotavora/eglot
(use-package eglot
  :disabled
  ;; :hook ((rustic-mode . eglot-ensure)) ;; rustic mode takes care of this
  :bind (:map eglot-mode-map ("C-c r" . #'eglot-rename)
			  ("M-RET" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown t))

(use-package consult-eglot
  :disabled
  :bind (:map eglot-mode-map ("M-g s" . #'consult-lsp-symbols)))

;; Auctex
(use-package auctex
  :disabled
  ;; :hook ((LaTeX-mode . (auto-fill-mode flyspell-mode LaTeX-math-mode)))
  :init
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTex-mode-hook 'auto-fill-mode)
  (add-hook 'LaTex-mode-hook 'flyspell-mode)
  (add-hook 'LaTex-mode-hook 'LaTeX-math-mode)
  (setq TeX-auto-save t
		TeX-parse-self t
		Tex-PDF-mode t
		TeX-view-program-selection '((output-pdf "PDF Tools"))
		TeX-source-correlate-start-server t)
  (setq-default TeX-master nil))

;; https://github.com/brotzeit/rustic
(use-package rustic
  :bind
  (:map rustic-mode-map
		("C-c <tab>" . 'rustic-popup)
		("C-c C-<tab>" . 'rustic-popup)
		("C-c C-p" . nil)
		("M-g d" . 'lsp-rust-analyzer-open-external-docs)
		("C-c C-c c" . nil)) ;; cleaning is bad
  :config
  (add-hook 'rustic-mode-hook 'tree-sitter-hl-mode)
  (add-hook 'rustic-mode-hook 'yas-minor-mode)
  (setq rustic-lsp-client 'lsp-mode
		rustic-format-trigger 'on-save
		rustic-format-display-method 'ignore))

(setq-default display-buffer-reuse-frames t)

;; Always open compilation buffers in the same window.
(add-to-list 'display-buffer-alist
			 (cons (lambda (buffer alist)
					 (with-current-buffer buffer
					   (eq major-mode 'rust-compilation)))
				   (cons 'display-buffer-reuse-major-mode-window
						 '((inhibit-same-window . nil)
						   (reusable-frames . visible)
						   (inhibit-switch-frame . nil)))))

;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :disabled
  :defer t
  :after lsp
  :hook ((java-mode . lsp)
		 (java-mode . (lambda ()
						(setq indent-tabs-mode nil)))))

;;; C-a move-beginning-of-line-or-indentation
(defun at-or-before-indentation-p ()
  (save-excursion
	(let ((old-point (point)))
	  (back-to-indentation)
	  (<= old-point (point)))))

(defun move-beginning-of-line-or-indentation () (interactive)
	   "If at the begining of line go to previous line.
 If at the indention go to begining of line. Go to indention
 otherwise."
	   (cond ((bolp) (forward-line -1))
			 ((at-or-before-indentation-p) (move-beginning-of-line nil))
			 (t (back-to-indentation))))


;;; C-e move-end-of-line-or-indentation
(defun at-or-after-indentation-p ()
  (save-excursion
	(let ((old-point (point)))
	  (back-to-indentation)
	  (>= old-point (point)))))

(defun move-end-of-line-or-indentation () (interactive)
	   "If at end of line go to next line.
If at indentation go to end of line.
Go to indentation otherwise"
	   (cond ((eolp) (forward-line 1))
			 ((at-or-after-indentation-p) (move-end-of-line nil))
			 (t (back-to-indentation))))


(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
	(select-window (active-minibuffer-window))))


;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
	(user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
				 (concat "/" (file-remote-p file 'method) ":"
						 (file-remote-p file 'user) "@" (file-remote-p file 'host)
						 "|sudo:root@"
						 (file-remote-p file 'host) ":" (file-remote-p file 'localname))
			   (concat "/sudo:root@localhost:" file))))


;; open terminal
(defun open-alacritty-in-workdir ()
  "Open an alacritty in the current folder"
  (interactive)
  (let ((default-directory (expand-file-name (project-root (project-current t)))))
	(call-process-shell-command
	 (concat "alacritty --working-directory=" default-directory) nil 0)))

;; stolen, no, borrowed from:
;; https://github.com/karthink/.emacs.d/blob/0d56c66c2e2d53ba05366493f433e523cc36cd87/lisp/setup-consult.el
;; https://github.com/minad/consult/wiki#find-files-using-fd
(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
	(setq consult--fd-command
		  (if (eq 0 (call-process-shell-command "fdfind"))
			  "fdfind"
			"fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
			   (`(,re . ,hl) (funcall consult--regexp-compiler
									  arg 'extended t)))
	(when re
	  (list :command (append
					  (list consult--fd-command
							"--color=never" "--full-path"
							(consult--join-regexps re 'extended))
					  opts)
			:highlight hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
		 (default-directory (cdr prompt-dir)))
	(call-interactively
	 #'find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))

(defun org-in-tangle-dir (sub-path)
  "Expand the SUB-PATH into the directory given by the tangle-dir
property if that property exists, else use the
`default-directory'."
  (expand-file-name sub-path
					(or
					 (org-entry-get (point) "tangle-dir" 'inherit)
					 (default-directory))))

;; (defalias 'mp-rust-windowing
  ;; (kmacro "C-x 3 C-x 3 C-x + M-o d C-x 2 M-o f C-u 1 5 M-x s h r i <return> M-o s"))

(defun display-buffer-reuse-major-mode-window (buffer alist)
  "Return a window displaying a buffer in BUFFER's major mode.
Return nil if no usable window is found.

If ALIST has a non-nil `inhibit-same-window' entry, the selected
window is not eligible for reuse.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable window:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

If ALIST contains no `reusable-frames' entry, search just the
selected frame if `display-buffer-reuse-frames' and
`pop-up-frames' are both nil; search all frames on the current
terminal if either of those variables is non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that a window on another frame is chosen, avoid raising
that frame."
  (let* ((alist-entry (assq 'reusable-frames alist))
		 (frames (cond (alist-entry (cdr alist-entry))
					   ((if (eq pop-up-frames 'graphic-only)
							(display-graphic-p)
						  pop-up-frames)
						0)
					   (display-buffer-reuse-frames 0)
					   (t (last-nonminibuffer-frame))))
		 (window (let ((mode (with-current-buffer buffer major-mode)))
				   (if (and (eq mode (with-current-buffer (window-buffer)
									   major-mode))
							(not (cdr (assq 'inhibit-same-window alist))))
					   (selected-window)
					 (catch 'window
					   (walk-windows
						(lambda (w)
						  (and (window-live-p w)
							   (eq mode (with-current-buffer (window-buffer w)
										  major-mode))
							   (not (eq w (selected-window)))
							   (throw 'window w)))
						'nomini frames))))))
	(when (window-live-p window)
	  (prog1 (window--display-buffer buffer window 'reuse alist)
		(unless (cdr (assq 'inhibit-switch-frame alist))
		  (window--maybe-raise-frame (window-frame window)))))))

;;; init.el ends here
