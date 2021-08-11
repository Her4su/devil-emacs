;;; init-media.el --- Media utlity
;;; Commentary:
;; require `mpv', also `pdf-tools-install'

(use-package emms
  :bind (("C-c 1" . emms-transient))
  :preface
  (transient-define-prefix emms-transient ()
    "Emms Player"
    :transient-suffix 'transient--do-stay
    ["Player"
     ("d" "play" emms-play-directory)
     ("r" "random" emms-random)
     ("n" "next" emms-next)
     ("p" "previous" emms-previous)
     ("f" "show" emms-show)
     ("s" "stop" emms-pause)
     ("+" "volume up" emms-volume-raise)
     ("-" "volume down" emms-volume-lower)])
  :custom
  (emms-source-file-default-directory "~/Music/")
  :config
  (emms-all)
  (emms-default-players))

;; read pdf handy
;; only recomment under linux, others are too slow
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf$" . pdf-view-mode)
  :hook (pdf-view-mode-hook . (lambda() (linum-mode -1)))
  :config
  (setq-default pdf-view-display-size 'fit-width)
  :bind (:map pdf-view-mode-map
   ("<s-spc>" . pdf-view-scroll-down-or-next-page)
   ("g"  . pdf-view-first-page)
   ("G"  . pdf-view-last-page)
   ("l"  . image-forward-hscroll)
   ("h"  . image-backward-hscroll)
   ("j"  . pdf-view-next-page)
   ("k"  . pdf-view-previous-page)
   ("e"  . pdf-view-goto-page)
   ("u"  . pdf-view-revert-buffer)
   ("al" . pdf-annot-list-annotations)
   ("ad" . pdf-annot-delete)
   ("aa" . pdf-annot-attachment-dired)
   ("am" . pdf-annot-add-markup-annotation)
   ("at" . pdf-annot-add-text-annotation)
   ("y"  . pdf-view-kill-ring-save)
   ("i"  . pdf-misc-display-metadata)
   ("s"  . pdf-occur)
   ("b"  . pdf-view-set-slice-from-bounding-box)
   ("r"  . pdf-view-reset-slice)))

(provide 'init-media)
;;; init-media.el ends here
