;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-appearence)
(require 'init-packages)
(require 'init-general)
(require 'init-evil)
(require 'init-org)
(require 'init-tex)
(require 'init-ocaml)

;; Custom Configurations

(setq custom-file (expand-file-name "~/.emacs.d/lisp/custom.el"))
(load custom-file 'no-error 'no-message)
