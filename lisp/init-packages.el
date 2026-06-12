;;; lisp/init-packages.el --- initialize packaging features for emacs

;; Install use-package
(setq load-prefer-newer t)
(require 'package)
(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-install-upgrade-built-in t)

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
