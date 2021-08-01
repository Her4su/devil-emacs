;;; init-search.el --- search configuration
;;; Commentary:
;; fuzzy complement framework

;;; Code:

(use-package vertico
  :straight (vertico :includes vertico-directory
                     :files (:defaults "extensions/*.el"))
  :bind (("C-c v" . vertico-repeat)
         (:map vertico-map
               ("M-RET" . minibuffer-force-complete-and-exit)))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless)))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package affe
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c H" . consult-history)
         ("C-c m" . consult-mode-command)
         ([remap bookmark-jump] . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ([remap recentf-open-files] . consult-recent-file)
         ;; C-x bindings (ctl-x-map)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ([remap abbrev-prefix-mark] . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ([remap yank-pop] . consult-yank-pop)
         ([remap apropos-command] . consult-apropos)
         ;; M-g bindings (goto-map)
         ([remap compile-goto-error] . consult-compile-error)
         ([remap flymake-show-diagnostics-buffer] . consult-flymake)
         ([remap goto-line] . consult-goto-line)             ;; orig. goto-line
         ([remap imenu] . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("C-c ," . consult-imenu-multi)
         ("C-."   . xref-find-references)
         ;; M-s bindings (search-map)
         ([remap project-find-file] . consult-find)
         ([remap locate] . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ([remap project-find-regexp] . consult-ripgrep)
         ([remap isearch-forward] . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ([remap isearch-edit-string] . consult-isearch)
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package embark
  :ensure t
  :bind
  (("C-c e" . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-search)
;;; init-search.el ends here
