;;; lisp/init-org.el --- Org Configuration

;; org-agenda-files
(defvar wearry/org-agenda-path
  "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
  "Primary org-agenda directory")

(defvar wearry/org-agenda-files
  `("~/Projects/blog/blogs.org"
    ,(expand-file-name "todo.org" wearry/org-agenda-path)
    ,(expand-file-name "habit.org" wearry/org-agenda-path))
  "Org-agenda file list")

(defvar wearry/org-journal-dir
  (expand-file-name "journal/" wearry/org-agenda-path)
  "Weekly journal directory")

;; org-mode cores
(use-package org
  :ensure t
  :defer t
  :commands (org-agenda
	     org-capture
	     org-store-link)
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :init
  (require 'org-habit)
  (require 'org-checklist)
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
	org-log-done t
	org-log-into-drawer t
	org-clock-into-drawer "LOGBOOK"
	org-habit-show-habits-only-for-today nil      ;; 显示整周打卡图
	org-habit-graph-column 62                     ;; 图形开始列位置
	org-habit-preceding-days 3
	org-habit-following-days 7
	org-columns-default-format "%45ITEM(Task) %5PRIORITY %10CLOCKSUM %24CLOSED")
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
  :config
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
    (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))
  ;; org-capture templates
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
		 `("H" "Habit" entry
		   (file+headline ,(expand-file-name
				    "habit.org"
				    wearry/org-agenda-path)
				  "Habits")
		   "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a>>\n:PROPERTIES:\n:STYLE: habit\n\
:REPEAT_TO_STATE: TODO\n:END:\n"))
    (add-to-list 'org-capture-templates
		 `("w" "Weekly Journal" entry
		   (file ,(expand-file-name
			   (format-time-string "%Y-W%V.org")
			   wearry/org-journal-dir))))))

(provide 'init-org)
