;;; lisp/init-packages.el --- initialize packaging features for emacs

;; Install use-package
(require 'package)
(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-install-upgrade-built-in t)

;; Bootstrap `use-package'
(require 'use-package)

;; Update-packages every 14 days automatically
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-interval 14
        auto-package-update-hide-results nil
        auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init-packages)
