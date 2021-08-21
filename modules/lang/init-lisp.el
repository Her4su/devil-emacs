;;; init-lisp.el --- Lisp
;;; Commentary:
;; require https://github.com/roswell/roswell/

;;; Code:

(use-package sly-asdf
  :defer t
  :after (sly company))

(use-package sly-quicklisp
  :after sly)

(use-package sly
  :custom
  (inferior-lisp-program my-lisp-compiler)
  :init
  (setq sly-complete-symbol*-fancy t)
  (setq sly-contribs '(sly-fancy
                         sly-indentation
                         sly-autodoc
                         sly-sbcl-exts
                         sly-scratch))
  :config
  (sly-setup '(sly-fancy sly-asdf sly-quicklisp)))

(use-package common-lisp-snippets
  :defer t
  :after (yasnippet))

(use-package geiser-guile
  :hook ((geiser-mode . paredit-mode))
  :bind (:map
         paredit-mode-map
         ("[" . paredit-open-round)
         ("(" . paredit-open-square)
         ("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)))

(provide 'init-lisp)
;;; init-lisp.el ends here
