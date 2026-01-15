;;; lisp/init-evil.el --- Init Evil Mode

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config (evil-mode t)
  :bind
  (:map evil-normal-state-map
	("/" . 'consult-line)
	("'" . 'consult-imenu)
	("-" . 'delete-other-windows)
	("`" . 'eshell)
	("[b" . 'switch-to-prev-buffer)
	("]b" . 'switch-to-next-buffer)
	("C-w" . 'kill-current-buffer)
	("C-e" . 'find-file)
	("C-f" . 'consult-find)
	("C-b" . 'consult-buffer)
	("C-l" . 'consult-locate)
	("C-<return>" . 'consult-grep)
	("SPC n" . 'evil-window-new)
	("SPC v" . 'evil-window-vnew)
	("SPC h" . 'evil-window-left)
	("SPC j" . 'evil-window-down)
	("SPC k" . 'evil-window-up)
	("SPC l" . 'evil-window-right)
	("SPC x" . 'kill-buffer-and-window)
	("SPC SPC" . 'execute-extended-command)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t
  :init
  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map
		(kbd "C-'") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map
		(kbd "C-'") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-insert-state-map
		(kbd "C-'") 'evilnc-comment-or-uncomment-lines)))

;; Unbind whitespaces
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "SPC") nil))

(provide 'init-evil)
