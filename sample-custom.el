;;; custom.el --- sample custom

;;; Commentary:
;; customize your Emacs

;;; Code:
(setq
 my-scheme '(guile)
 my-font-name "JetBrains Mono" ; default font
 my-org-latex-scale 1.0 ; latex scale
 my-font-size "10" ; font size
 my-use-linum nil ; show line number in side bar
 my-lisp-compiler "sbcl"
 my-ui-theme 'doom-tomorrow-day
 my-detailed-ui t
 my-extra-files '()
 my-modules ; the langauges you want to enable
 '(; a list of features to turn on
   ; modal      ;; modal editing
   ; media      ;; media player
   ; commu      ;; communications
   agda       ;; Agda theorem prover
   lisp       ;; Common Lisp
   coq        ;; Coq theorem prover
   cpp        ;; C/C++
   julia      ;; Julia
   haskell    ;; Haskell
   idris      ;; Idris
   latex      ;; Latex
   ocaml      ;; OCaml
   rust       ;; Rust
   erlang     ;; Erlang
   ))

(setq debug-on-error nil)

(provide 'custom.el)
;;; custom.el ends here
