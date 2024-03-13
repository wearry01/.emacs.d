;; Init Packages

(require 'package)
(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Setup `use-package'

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(provide 'init-packages)
