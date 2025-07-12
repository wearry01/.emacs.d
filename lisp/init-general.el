;;; lisp/init-general.el --- General Setting

;; Basic Properties

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)
(setq make-backup-files nil)
(setq tab-always-indent 'complete)
(setq ring-bell-function 'ignore)

(add-hook 'before-save-hook 'time-stamp)

;; Global Keybindings

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f5>") 'open-init-file)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-s") 'save-buffer)

;; Completion & Navigation Packages

(use-package restart-emacs :ensure t)

(use-package recentf
  :ensure t
  :hook (after-init . recentf-mode)
  :config (setq recentf-max-menu-items 8))

(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	      history-length 1024
	      savehist-additional-variables '(mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring
					      extended-command-history)
	      savehist-autosave-interval 300))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . 'company-select-next)
              ("C-p" . 'company-select-previous))
  :config
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.15
        company-tooltip-align-annotations t))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil))

(use-package wgrep
  :ensure t
  :config (setq wgrep-auto-save-buffer t))

(use-package consult
  :ensure t
  :config (setq consult-locate-args "mdfind -name"))

(use-package embark
  :ensure t
  :bind (("C-;" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.5))

(provide 'init-general)
