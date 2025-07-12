;;; lisp/init-packages.el --- initialize packaging features for emacs

;; Install use-package
(require 'package)
(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Update-packages every 10 days automatically
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-interval 10
        auto-package-update-prompt-before-update t  ;; 是否弹出确认
        auto-package-update-hide-results nil
        auto-package-update-delete-old-versions t)  ;; 删除旧版本
  (auto-package-update-maybe))

(unless (server-running-p)
  (server-start))

(provide 'init-packages)
