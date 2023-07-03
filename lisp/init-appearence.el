;; Appearence Setting

(global-display-line-numbers-mode)
(global-hl-line-mode)
(electric-pair-mode)
(global-auto-revert-mode)

(setq inhibit-startup-screen t)
(set-scroll-bar-mode nil)
(set-face-attribute 'default nil :height 140)

(use-package timu-macos-theme
  :ensure t
  :when (display-graphic-p)
  :config (load-theme 'timu-macos t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

(provide 'init-appearence)
