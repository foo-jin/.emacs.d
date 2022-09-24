;;; lang-modes.el --- Config for specific programming major-modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/brotzeit/rustic
(use-package rustic
  :hook ((rustic-mode . yas-minor-mode)
		 ;; (rustic-mode . eglot-ensure)
		 ;; (before-save . (lambda () (when (eq 'rustic-mode major-mode)
									 ;; (lsp-format-buffer))))
		 (rustic-mode . tree-sitter-hl-mode))
  :bind
  (:map rustic-mode-map
		("C-c <tab>" . rustic-popup)
		("C-c C-<tab>" . rustic-popup)
		("C-c C-p" . nil)
		("M-g d" . lsp-rust-analyzer-open-external-docs)
		("C-c C-c c" . nil))
  :config
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


;; https://github.com/juergenhoetzel/pkgbuild-mode
(use-package pkgbuild-mode)
(use-package yaml-mode
  :delight)


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

;; https://github.com/politza/pdf-tools
;; M-x pdf-tools-install RET
(use-package pdf-tools
  :config
  (pdf-tools-install))


;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :defer t
  :after lsp
  :hook ((java-mode . lsp)
		 (java-mode . (lambda ()
						(setq indent-tabs-mode nil)))))


(provide 'lang-modes)
;;; lang-modes.el ends here
