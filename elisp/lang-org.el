;;; lang-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :hook ((org-mode . visual-line-mode)
		 (org-mode . variable-pitch-mode)
		 (org-mode . flyspell-mode)
		 (org-mode . yas-minor-mode))
  :config
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
				  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
		;; org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
		;; 		       ("NEXT" :foreground "deep sky blue" :weight bold)
		;; 		       ("DONE" :foreground "forest green" :weight bold)
		;; 		       ("WAITING" :foreground "orange" :weight bold)
		;; 		       ("HOLD" :foreground "magenta" :weight bold)
		;; 		       ("CANCELLED" :foreground "forest green" :weight bold)))
		org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
					     ("WAITING" ("WAITING" . t))
					     ("HOLD" ("WAITING") ("HOLD" . t))
					     (done ("WAITING") ("HOLD"))
					     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
					     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
					     ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
		org-capture-templates '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
								"* TODO %?\nAdded: %U\n %i"))
		org-use-fast-todo-selection t
		org-startup-indented t
		org-ellipsis "  "
		org-pretty-entities t
		org-hide-emphasis-markers t
		org-special-ctrl-a/e t
		org-hide-leading-stars t
		org-highlight-latex-and-related '(latex))

  ;; org-babel stuff
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((python . t)
		(shell . t)))
  (setq org-src-fontify-natively t
		org-src-window-setup 'current-window)

  ;; simple bullets
  (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  :bind
  ("C-c C-l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))

(use-package ob-async)

(use-package cdlatex
  :after (org)
  :init (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/documents/zet")
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n t a" . org-roam-tag-add)
		 ("C-c n t r" . org-roam-tag-remove)
		 ("C-c n a a" . org-roam-alias-add)
		 ("C-c n a r" . org-roam-alias-remove))
  :config
  (setq org-roam-capture-templates
		'(("d" "default" plain "- tags :: %?"
		   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							  "#+title: ${title}\n#+created: %U\n#+last_modified: %U")
		   :empty-lines-before 1
		   :unnarrowed t)))
  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (org-roam-setup))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(provide 'lang-org)
;;; lang-org.el ends here
