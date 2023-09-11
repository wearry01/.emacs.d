;; Init Evil Mode

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :hook (after-init . evil-mode)
  :bind (:map evil-normal-state-map
	      ("H" . 'evil-first-non-blank)
	      ("L" . 'evil-last-non-blank)
	      ("/" . 'consult-line)
	      ("'" . 'consult-imenu)
	      ("-" . 'delete-other-windows)
	      ("`" . 'eshell)
	      ("[b" . 'switch-to-prev-buffer)
	      ("]b" . 'switch-to-next-buffer)
	      ("C-e" . 'find-file)
	      ("C-b" . 'consult-buffer)
	      ("C-l" . 'consult-locate)
	      ("C-r" . 'consult-ripgrep)
	      ("SPC n" . 'evil-window-new)
	      ("SPC v" . 'evil-window-vnew)
	      ("SPC h" . 'evil-window-left)
	      ("SPC j" . 'evil-window-down)
	      ("SPC k" . 'evil-window-up)
	      ("SPC l" . 'evil-window-right)
	      ([remap evil-quit] . kill-buffer-and-window)))


;; Unbind `Tab'

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil))

(provide 'init-evil)
