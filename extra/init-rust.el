;;; init-rust.el --- Rust
;;; Commentary:
;; require `rust-analyzer'

;;; Code:

(use-package rust-mode
  :init (setq rust-format-on-save t)
  :hook (rust-mode . eglot-ensure)
  :config
  (use-package cargo
    :diminish cargo-minor-mode
    :hook (rust-mode . cargo-minor-mode)))

(provide 'init-rust)
;;; init-rust.el ends here
