;; General Setting

;;; Basic Properties

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)
(setq make-backup-files nil)
(setq tab-always-indent 'complete)
(setq ring-bell-function 'ignore)

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f5>") 'open-init-file)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-x") 'kill-region)

;;; Package Configurations

(use-package restart-emacs :ensure t)

(use-package recentf
  :ensure t
  :init (recentf-mode)
  :config (setq recentf-max-menu-items 6))

(use-package company
  :ensure t
  :bind (:map company-active-map
	      ("C-n" . 'company-select-next)
	      ("C-p" . 'company-select-previous))
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.1)
  (setq company-selection-wrap-around t))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless)))

(use-package wgrep
  :ensure t
  :config (setq wgrep-auto-save-buffer t))

(use-package consult
  :ensure t
  :bind (("C-c s" . 'consult-ripgrep)
	 ("C-c l" . 'consult-locate))
  :config (setq consult-locate-args "mdfind -name"))

(use-package embark
  :ensure t
  :init (use-package wgrep :ensure t)
  :bind (("C-;" . 'embark-act)
	 :map minibuffer-local-map
	 ("C-r" . 'embark-export-write))
  :config
  (setq prefix-help-command 'embark-prefix-help-command)
  (defun embark-export-write()
    "Export the current vertico results to a writable buffer is possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (pcase-let ((`(,type . ,candidates)
		 (run-hook-with-args-until-success 'embark-candidate-collectors)))
      (pcase type
	('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
			 (embark-export)))
	('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
		 (embark-export)))
	('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
			     (embark-export)))
	(x (user-error "embark category %S doesn't support writable export" x)))))
  (eval-after-load 'consult
    '(eval-after-load 'embark
       '(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))

(use-package embark-consult
  :ensure t
  :after embark consult)

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	      history-length 1000
	      savehist-additional-variables '(mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring
					      extended-command-history)
	      savehist-autosave-interval 300))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(provide 'init-general)
