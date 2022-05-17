;;; base-functions.el --- Custom elisp functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Add your custom functions here

;; (defun something
;;    (do-something))
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
  (let ((default-directory (project-root (project-current t))))
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
			  "fdfind")
		  "fd"))
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
    (call-interactively #'find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

(provide 'base-functions)
;;; base-functions.el ends here
