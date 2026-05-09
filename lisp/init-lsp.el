;;; lisp/init-lsp.el --- LSP Configuration

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((LaTeX-mode . lsp-deferred)
         (latex-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (python-mode . lsp-deferred))
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-completion-provider :capf)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-log-io nil)
  (lsp-restart 'auto-restart))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-peek-enable t))

(provide 'init-lsp)
