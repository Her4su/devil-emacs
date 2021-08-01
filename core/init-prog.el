;;; init-prog.el --- The completion engine and language server
;;; Commentary:
;; programming settings

;;; Code:
(require 'init-const)

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my/company-yasnippet)
         ("<C-return>" . company-complete-selection)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :preface
  (defun my/company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  (defun my/company-complete ()
    "Bring up the completion popup. If only one result, complete it."
    (interactive)
    (require 'company)
    (when (ignore-errors
            (/= (point)
                (cdr (bounds-of-thing-at-point 'symbol))))
      (save-excursion (insert " ")))
    (when (and (company-manual-begin)
               (= company-candidates-length 1))
      (company-complete-common)))
  :init
  (setq
   company-tooltip-align-annotations t
   company-tooltip-limit 12
   company-idle-delay 0
   company-echo-delay (if (display-graphic-p) nil 0)
   company-minimum-prefix-length 3
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-show-numbers t
   company-dabbrev-downcase nil
   company-global-modes '(not
                          erc-mode message-mode help-mode
                          gud-mode eshell-mode shell-mode)
   company-backends '(company-capf company-files)
   company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend)))

(use-package company-quickhelp
  :after company
  :hook (global-company-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.5))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :bind (("C-c y" . yas-visit-snippet-file))
  :config
  (use-package yasnippet-snippets))

(use-package eglot
  :bind (("C-c =" . eglot-format)
         ("C-c -" . eglot-rename)
         ("C-c 0" . eglot-code-actions)
         ("C-c h" . egdoc)))

(provide 'init-prog)
;;; init-prog.el ends here
