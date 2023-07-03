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
	      ("?" . 'consult-locate)
	      ("-" . 'delete-other-windows)
	      ("`" . 'eshell)
	      ("[b" . 'switch-to-prev-buffer)
	      ("]b" . 'switch-to-next-buffer)
	      ("C-b" . 'consult-buffer)
	      ("C-e" . 'find-file)
	      ("SPC n" . 'evil-window-split)
	      ("SPC v" . 'evil-window-vsplit)
	      ("SPC h" . 'evil-window-left)
	      ("SPC j" . 'evil-window-down)
	      ("SPC k" . 'evil-window-up)
	      ("SPC l" . 'evil-window-right)
	      ([remap evil-quit] . delete-window)))

(provide 'init-evil)
