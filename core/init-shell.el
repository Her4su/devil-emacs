;;; init-shell.el --- shell configurations
;;; Commentary:
;;

;;; Code:

(use-package eshell
  :preface
  :config
  (setq
   eshell-highlight-prompt nil
   eshell-smart t
   eshell-buffer-shorthand t
   eshell-cmpl-ignore-case t
   eshell-cmpl-cycle-completions t
   eshell-destroy-buffer-when-process-dies t
   eshell-history-size 10000
   ;; auto truncate after 20k lines
   eshell-buffer-maximum-lines 20000
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-glob-case-insensitive t
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all
   eshell-list-files-after-cd t
   eshell-banner-message "")
  (use-package eshell-prompt-extras
    :after esh-opt
    :config
    (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
    (setq eshell-prompt-function 'epe-theme-lambda))
  (use-package eshell-syntax-highlighting
    :after esh-mode
    :demand t
    :config
    (eshell-syntax-highlighting-global-mode +1)))
  
(use-package shell
  :ensure nil)

;; Shell Pop
(use-package shell-pop
  :bind (("<f9>" . shell-pop))
  :custom
  (shell-pop-window-size 35)
  (shell-pop-shell-type
   '("eshell" "*eshell*" (lambda ()
                           (eshell)))))
     
(provide 'init-shell)
;;; init-shell.el ends here
