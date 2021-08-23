;;; init-const.el --- constatn might be used later
;;; Commentary:
;; Some constants which might be used later

;;; Code:
;; customized constants
(defgroup my-setting nil
  "My Emacs config setting"
  :group 'convenience)

(defcustom my-scheme '(chez)
  "Scheme default implementation."
  :type '(symbol list)
  :group 'my-setting)

(defcustom my-font-name "JetBrains Mono"
  "My font name."
  :type '(string)
  :group 'my-setting)

(defcustom my-ui-theme 'doom-one-light
  "My font name."
  :type '(symbol)
  :group 'my-setting)

(defcustom my-font-size "10"
  "My font size."
  :group 'my-setting
  :type '(integer))

(defcustom my-use-linum nil
  "Show line number."
  :group 'my-setting
  :type '(boolean))

(defcustom my-detailed-ui t
  "Use mininum UI by default."
  :group 'my-setting
  :type '(boolean))

(defcustom my-modules
  '()
  "The list of modules to enable."
  :group 'my-setting
  :type '(list symbol))

(defcustom my-extra-files
  '()
  "The list of customize mode files."
  :group 'my-setting
  :type '(list symbol))

(defcustom my-lisp-compiler "sbcl"
  "Define Lisp Implementation."
  :group 'my-setting
  :type '(string))

(defcustom my-org-latex-scale 1.0
  "The scaling for latex rendering in org mode"
  :group 'my-setting
  :type '(integer))

;; helper constants
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defmacro my/load-module (lang)
  "Load a LANG when it's in language list."
  (when (member lang my-modules)
    (let ((lang-file
           (intern
            (concat "init-" (symbol-name lang)))))
      `(require ',lang-file))))

(defun my/load-extras ()
  "Load the user's customized files"
  (mapcar (lambda (my-file) (require my-file))
          my-extra-files))

;; check if a font exits
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun check-module (list)
  "Check if a module installed cooreponding LIST."
  (let ((module-name (first list))
        (dependencies (rest list)))
    (seq-remove (lambda (e) (executable-find e)) dependencies)))

(defun my/font-info ()
  "Get the current font name with size."
  (concat my-font-name " " my-font-size))

;; load custom file
(progn
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory)) 
  (unless (file-exists-p custom-file)
    (copy-file
     (expand-file-name "sample-custom.el" user-emacs-directory)
     custom-file))  
  (load-file custom-file))

(provide 'init-const)
;;; init-const.el ends here
