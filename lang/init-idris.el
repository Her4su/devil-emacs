;;; init-idris.el --- Idris
;;; Commentary:
;; require `idris'

;;; Code:
(use-package idris2-mode
  :straight (idris2-mode :type git :host github
                         :repo "redfish64/idris2-mode"))

(provide 'init-idris)
;;; init-idris.el ends here
