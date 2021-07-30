;;; custom.el --- sample custom

;;; Commentary:
;; customize your Emacs

;;; Code:
(setq
 my-scheme '(guile)
 my-font-name "JetBrains Mono" ; default font
 my-font-size "10" ; font size
 my-use-linum nil ; show line number in side bar
 my-use-which-key t ; show key hints
 my-use-evil t ; use evil
 my-lisp-compiler "sbcl"
 my-ui-theme 'doom-one-light
 my-language-list ; the langauges you want to enable
 '(agda clisp coq cpp julia haskell idris latex ocaml racket rust erlang))

(setq debug-on-error nil)

(provide 'custom.el)
;;; custom.el ends here
