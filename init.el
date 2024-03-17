;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-packages)
(require 'init-appearence)
(require 'init-general)
(require 'init-evil)
(require 'init-org)
(require 'init-tex)

;; Custom Configurations

(setq custom-file (expand-file-name "~/.emacs.d/lisp/custom.el"))
(load custom-file 'no-error 'no-message)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
