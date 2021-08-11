;;; init.el --- initialize all emacs settings

;;; Commentary:
;; The startup file
;; Emacs config by Galois Neko :P
;; Special thanks to seagle0128, purcell, and doom-emacs

;;; Code:
;; version checking >= 28

;; comment out for debugging
(setq debug-on-error nil)

;; initialize straight
(defvar bootstrap-version)

(setq straight-vc-git-default-clone-depth 1
      straight-check-for-modifications nil
      straight-use-package-by-default t
      straight-recipes-gnu-elpa-use-mirror t)

;; loading bootstrap file
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; should set before loading `use-package'
(eval-and-compile
  (setq use-package-defaults nil)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

;; use `use-package' macro to beautify config
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

;; required by `use-package'
(use-package diminish)
(use-package bind-key)

;; profiler
(use-package esup
  :defer 1
  :commands (esup))

;; load the customized config
;; core for core utility such as UI and completions
;; lang for programming language settings
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "extra" user-emacs-directory))

;; initialize constants and helper functions
(require 'init-const)
(require 'init-boot)

;;; better editing
(require 'init-edit)
(require 'init-search)
(require 'init-workspace)

;;; better UI
(require 'init-ui)
(require 'init-treemacs)

;;; better integration
(require 'init-shell)
(require 'init-note)
(require 'init-dev)
(require 'init-prog)
(require 'init-util)

;; Extra Emacs utlityies
(my-load-extra modal)
(my-load-extra media)

;;; programming languages environment
(my-load-extra cpp)
(my-load-extra agda)
(my-load-extra coq)
(my-load-extra ocaml)
(my-load-extra racket)
(my-load-extra clisp)
(my-load-extra idris)
(my-load-extra rust)
(my-load-extra haskell)
(my-load-extra latex)
(my-load-extra erlang)
(my-load-extra julia)
;;; init.el ends here
