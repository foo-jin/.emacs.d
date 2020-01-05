;;; lang-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :config
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
				  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
	org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
				       ("NEXT" :foreground "deep sky blue" :weight bold)
				       ("DONE" :foreground "forest green" :weight bold)
				       ("WAITING" :foreground "orange" :weight bold)
				       ("HOLD" :foreground "magenta" :weight bold)
				       ("CANCELLED" :foreground "forest green" :weight bold)))
	org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
					     ("WAITING" ("WAITING" . t))
					     ("HOLD" ("WAITING") ("HOLD" . t))
					     (done ("WAITING") ("HOLD"))
					     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
					     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
					     ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
	org-use-fast-todo-selection t
	org-startup-indented t
	org-ellipsis "  "
	org-pretty-entities t
	org-hide-emphasis-markers t)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

;; (use-package org-projectile
  ;; :delight
  ;; :after org
  ;; :config
  ;; (org-projectile-per-project)
  ;; (setq org-projectile-per-project-filepath "todo.org"
	;; org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :delight
  :hook ((org-mode . org-bullets-mode))
  :config
  (setq org-hide-leading-stars t
	org-bullets-bullet-list
	'("⊗" "✻" "☉" "❃" "○" "❆" "◦")))

(provide 'lang-org)
;;; lang-org.el ends here
