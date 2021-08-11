;;; init-coq.el --- Coq Theorem Prover
;;; Commentary:
;; require `coq' related binaries

;;; Code:

;; HACK for some reasons PG cannot be loaded with correct location
;; so set path manually
(when sys/mac-x-p
    (setq
     pg-init--script-full-path (concat user-emacs-directory "straight/repos/PG/proof-general.el")
     pg-init--pg-root (file-name-directory pg-init--script-full-path)))

(use-package proof-general
  :diminish (holes-mode)
  :mode ("\\.v$" . coq-mode)
  :custom
  (proof-splash-enable nil)
  (proof-three-window-enable t)
  (coq-project-filename "_CoqProject")
  :init
  (when (bound-and-true-p meow-mode)
    (setq meow-mode-state-list (push '(coq-mode . normal) meow-mode-state-list))))

(use-package company-coq
  :diminish
  :hook (coq-mode . company-coq-mode)
  :custom
  (company-coq-features/prettify-symbols-in-terminals t))

(provide 'init-coq)
;;; init-coq ends here
