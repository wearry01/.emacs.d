;;; lisp/init-packages.el --- initialize packaging features for emacs

(setq native-comp-jit-compilation t)
(setq native-comp-async-report-warnings-errors 'silent)

;; Install use-package
(setq load-prefer-newer t)
(require 'package)
(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Bootstrap `use-package'
(require 'use-package)
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init-packages)
