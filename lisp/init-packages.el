;;; lisp/init-packages.el --- initialize packaging features for emacs

;; Install use-package
(setq load-prefer-newer t)
(require 'package)
(setq package-archives
      '(("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; Bootstrap `use-package'
(require 'use-package)

;; Recompile stale .elc before loading — prevents stale-byte-code errors
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode 1))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init-packages)
