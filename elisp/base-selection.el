;;; base-selection.el --- Config for incremental narrowing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/raxod502/selectrum
(use-package selectrum
  :config
  (selectrum-mode +1))

;; https://github.com/raxod502/prescient.el
(use-package prescient
  :config
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1))

;; (use-package company-prescient
  ;; :config
  ;; (company-prescient-mode))

;; https://github.com/oantolin/orderless
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (savehist-mode)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
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




;; https://github.com/minad/consult#configuration
(use-package consult
  :bind (("C-x b" . consult-buffer)
		 ("M-i" . consult-imenu)
		 ("M-y" . consult-yank-pop)
		 ("M-g o" . consult-outline)
		 ("M-g f" . consult-flymake)
		 ("M-g m" . consult-mark)
		 ("M-g M" . consult-global-mark)
		 ("M-s r" . consult-ripgrep)
		 ("M-s g" . consult-git-grep)
		 ("M-s f" . consult-fd)
		 ("M-s F" . consult-locate)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 :map isearch-mode-map
		 ("C-p" . consult-isearch-history))
  :config
  (when (executable-find "plocate")
	(setq consult-locate-args "plocate --ignore-case --existing --regexp"))
  (consult-customize
     consult-ripgrep consult-git-grep consult-grep
	 consult-bookmark consult-recent-file consult-xref
	 consult-compile-error
	 consult--source-file consult--source-project-file consult--source-bookmark
	 :preview-key '(:debounce 3 any)))

;; (use-package consult-flycheck
  ;; :after consult)

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
		 :map selectrum-minibuffer-map
		 ("C-x C-d" . consult-dir)
		 ("C-x C-j" . consult-dir-jump-file)))

;; (use-package embark
;;   :bind (("C-." . embark-act)
;; 		 ("C-," . embark-dwim)
;; 		 ("C-h B" . embark-bindings)
;; 		 :map embark-file-map
;; 		 ("s" . 'sudo-find-file))
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   (add-to-list 'display-buffer-alist
;; 			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;; 				 nil
;; 				 (window-parameters (mode-line-format . none)))))

;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))


(provide 'base-selection)
;;; base-selection.el ends here
