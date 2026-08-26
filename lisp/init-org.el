;;; lisp/init-org.el  -*- lexical-binding: t; -*-
;; --- Org Configuration

;; org-agenda-files
(defvar wearry/org-agenda-path
  "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
  "Primary org-agenda directory")

(defvar wearry/org-agenda-files
  `("~/Projects/blog/blogs.org"
    ,(expand-file-name "log.org" wearry/org-agenda-path)
    ,(expand-file-name "todo.org" wearry/org-agenda-path))
  "Org-agenda file list")

;; org-mode cores
(use-package org
  :ensure t
  :defer t
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (org-latex-preview '(16))))
  (org-babel-after-execute . org-display-inline-images)
  :commands (org-agenda
	     org-capture
	     org-store-link)
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
  (with-eval-after-load 'evil-maps
    (evil-define-key 'normal org-agenda-mode-map
      "vr" #'org-agenda-clockreport-mode))
  (setq org-agenda-files wearry/org-agenda-files)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "MEETING(m) WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
	org-todo-keyword-faces
	'(("TODO" :foreground "red" :weight bold)
	  ("NEXT" :foreground "cyan" :weight bold)
	  ("DONE" :foreground "green" :weight bold)
	  ("WAITING" :foreground "orange" :weight bold)
	  ("HOLD" :foreground "magenta" :weight bold)
	  ("CANCELLED" :foreground "grey" :weight bold)
	  ("MEETING" :foreground "forest green" :weight bold)))

  (setq org-startup-indented t
	org-hide-block-startup t
	org-hide-drawer-startup t
	org-log-done t
	org-log-into-drawer "LOGSTATE"
	org-clock-into-drawer "LOGBOOK"
	org-clock-mode-line-total 'today ;; 可选: today
	;; config preview
	org-preview-latex-default-process 'dvisvgm
	org-format-latex-options (plist-put org-format-latex-options :scale 1.212))

  (setq org-agenda-custom-commands
	'(("c" "Complete Agenda View"
	   ((agenda "" ((org-agenda-overriding-header "📅 Weekly Schedule")
			(org-deadline-warning-days 7)))
	    (tags "PROBLEM" ((org-agenda-overriding-header "🌟 Important Problems")
			     (org-agenda-skip-function
			      '(org-agenda-skip-entry-if 'notregexp "PRB"))))
	    (tags "BLOG" ((org-agenda-overriding-header "📃 Blog Posts")))
	    (todo "NEXT" ((org-agenda-overriding-header "🚀 Step Forward")))
	    (todo "MEETING" ((org-agenda-overriding-header "📌 Appointed Meeting")))))))

  ;; LaTeX & Beamer
  (require 'ox-beamer)
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("ctex" "\\documentclass[11pt]{ctexart}"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-default-packages-alist '("" "color" nil))
    (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))))

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode))

(provide 'init-org)
