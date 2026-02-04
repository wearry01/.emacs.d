;;; lisp/init-tex.el --- Init Tex Environment

(use-package preview-dvisvgm
  :ensure t)

(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex)
  :init
  (setq cdlatex-takeover-parenthesis nil
	cdlatex-paired-parens "${[("))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . TeX-source-correlate-mode)
	 (LaTeX-mode . outline-minor-mode))
  :config
  (setq-default TeX-master nil)
  (setq LaTeX-electric-left-right-brace t
	TeX-auto-save t
	TeX-parse-self t
	reftex-plug-into-AUCTeX t
	TeX-electric-math (cons "\\(" "\\)")
	TeX-electric-sub-and-superscript t
	TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list
	'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list '("LuaLaTeX" "%`lualatex%(mode)%' %t" TeX-run-TeX nil t)))

(provide 'init-tex)
