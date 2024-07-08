;; Init Tex Environment

(use-package preview-dvisvgm :ensure t)

(use-package cdlatex
  :ensure t
  :init
  (setq cdlatex-takeover-parenthesis nil)
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex))

(use-package tex
  :ensure auctex
  :init
  (require 'preview-dvisvgm)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list '("LuaLaTeX" "%`lualatex%(mode)%' %t" TeX-run-TeX nil t))
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . TeX-source-correlate-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-electric-sub-and-superscript t)
  (setq LaTeX-electric-left-right-brace t)
  (setq reftex-plug-into-AUCTeX t)
  ;; Config SyncTeX
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list
	'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer"))))

(server-start)
(provide 'init-tex)
