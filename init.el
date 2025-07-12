;;; init.el --- initialize emacs

;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-packages)
(require 'init-appearance)
(require 'init-general)
(require 'init-evil)
(require 'init-org)
(require 'init-org-ext)
(require 'init-tex)
(require 'init-llm)

;; Custom Configurations

(setq custom-file (expand-file-name "~/.emacs.d/lisp/custom.el"))
(load custom-file 'no-error 'no-message)
