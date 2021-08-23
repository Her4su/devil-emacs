;;; init-workspace.el --- Load back files
;;; Commentary:
;; Workspace for Emacs.

;;; Code:
(use-package popup)

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :hook (after-init . windmove-default-keybindings))

;; restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :custom (winner-dont-bind-my-key nil)
  :init (setq winner-boring-buffers
              '("*Completions*"
                "*Compile-Log*"
                "*inferior-lisp*"
                "*Fuzzy Completions*"
                "*Apropos*"
                "*Help*"
                "*cvs*"
                "*Buffer List*"
                "*Ibuffer*"
                "*esh command on file*")))

;; quickly switch windows
(use-package ace-window
  :bind
  (([remap other-window] . ace-window)
   ("M-o" . ace-window)
   ("C-x /" . split-window-right)
   ("C-x -" . split-window-below)))

;; store session information
(use-package persp-mode
  :diminish
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode))
  :init
  (setq-default persp-keymap-prefix (kbd "C-x p"))
  (setq
   persp-nil-name "default"
   persp-set-last-persp-for-new-frames nil
   persp-kill-foreign-buffer-behaviour 'kill
   persp-auto-resume-time 0)
  :config
  (add-to-list 'persp-common-buffer-filter-functions
               (lambda (b)
                 "Ignore temporary buffers."
                 (let ((bname (file-name-nondirectory (buffer-name b))))
                   (or (string-prefix-p "*" bname)
                       (string-prefix-p ".newsrc" bname)
                       (string-prefix-p "magit" bname)
                       (string-prefix-p "Pfuture-Callback" bname)
                       (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                       (eq (buffer-local-value 'major-mode b) 'erc-mode)
                       (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                       (eq (buffer-local-value 'major-mode b) 'nov-mode)
                       (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude)))

(provide 'init-workspace)
;;; init-workspace.el ends here
