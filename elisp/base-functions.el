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
  (call-process-shell-command
   (concat "alacritty --working-directory=" default-directory) nil 0))

(provide 'base-functions)
;;; base-functions.el ends here
