;; lisp/init-appearance.org --- Appearance Setting

(global-display-line-numbers-mode)
(global-hl-line-mode)
(electric-pair-mode)
(global-auto-revert-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(setq inhibit-startup-screen t)
(set-face-attribute 'default nil :font "Monaco" :height 144)

(use-package timu-macos-theme
  :ensure t
  :when (display-graphic-p)
  :config (load-theme 'timu-macos t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

(provide 'init-appearance)
