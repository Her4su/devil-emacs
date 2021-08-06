;;; init-dev.el --- development related tools
;;; Commentary:
;; currently includes git, docker, and projectile

;;; Code:
(require 'init-const)

;; compilation mode
(use-package compile
  :ensure nil
  :bind (("C-c c c" . compile)
         ("C-c c r" . recompile)
         ("C-c c e" . compile-goto-error))
  :custom
  (compilation-scroll-output t))

;; quickrun codes
(use-package quickrun
  :defer 1
  :bind (("C-c c x" . quickrun))
  :custom (quickrun-focus-p nil))

;; use magit for git VC
(use-package magit
  :bind (("C-x g" . magit-status)
          ("C-x M-g" . magit-dispatch)
          ("C-c M-g" . magit-file-popup))
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :config
  ;; access git forges from magit
  (when (executable-find "sqlite")
        (use-package forge)))

;; walk through git revisions of a file
(use-package git-timemachine
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

;; git related modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; project managemnt
(use-package project
  :ensure nil
  :init
  (define-key ctl-x-map "p" project-prefix-map))
 
;; use ripgrep to power up search speed
(use-package ripgrep
  :defer t)

;; syntax checking
(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  (text-mode . flymake-mode)
  :bind (("C-c f" . flymake-show-diagnostics-buffer)
         ("C-c [" . flymake-goto-prev-error)
         ("C-c ]" . flymake-goto-next-error)))

;; docker
(use-package dockerfile-mode)

(provide 'init-dev)
;;; init-dev.el ends here
