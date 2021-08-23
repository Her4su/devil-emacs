;;; init-commu.el --- communication in emacs
;;; Commentary:
;; `elfeed', `mu4e', `erc', the old school way 

(use-package elfeed
  :bind ("C-c 3" . elfeed))

; required: maildir-utils
(use-package mu4e
  :straight
  (:local-repo 
   "/usr/share/emacs/site-lisp/mu4e"
   :pre-build ())
  :custom
  (mu4e-maildir (expand-file-name "~/Documennts/mu4e"))
  (mu4e-attachment-dir (expand-file-name "~/Documents/mu4e/attach"))
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check t)
  (mu4e-hide-index-messages t)
  (mu4e-confirm-quit nil)
  (mu4e-compose-signature nil)
  :bind ("C-c 4" . mu4e)
  :custom
  (unless (file-directory-p "~/Documents/mu4e")
    (shell-command "mu init --maildir ~/Documents/mu4e")))

(provide 'init-commu)
;; init-commu.el ends here
