;;; lisp/init-org.el --- Init Org Mode

(defconst wearry/org-agenda-path
  "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org")

;; org-mode basics
(use-package org
  :ensure org-contrib
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :init
  (require 'org-checklist)
  (setq org-columns-default-format "%45ITEM(Task) %5PRIORITY %10CLOCKSUM %24CLOSED")
  (setq org-log-done t
	org-log-into-drawer t
	org-startup-indented t
	org-highlight-latex-and-related '(native entities)
	org-agenda-files (list wearry/org-agenda-path "~/Projects/blog/blogs.org")
	org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
			    (sequence "MEETING(m) WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
	org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
				 ("NEXT" :foreground "cyan" :weight bold)
				 ("DONE" :foreground "green" :weight bold)
				 ("WAITING" :foreground "orange" :weight bold)
				 ("HOLD" :foreground "magenta" :weight bold)
				 ("CANCELLED" :foreground "grey" :weight bold)
				 ("MEETING" :foreground "forest green" :weight bold)))
  (setq org-agenda-custom-commands
	'(("c" "Complete Agenda View"
	   ((agenda "" ((org-agenda-overriding-header "Weekly Schedule")
			(org-deadline-warning-days 7)))
	    (tags "PROJECT" ((org-agenda-overriding-header "Working Projects")
			     (org-agenda-skip-function
			      '(org-agenda-skip-entry-if 'notregexp "PRJ"))))
	    (tags "BLOG" ((org-agenda-overriding-header "Blog Posts")))
	    (todo "NEXT" ((org-agenda-overriding-header "Step Forward")))
	    (todo "MEETING" ((org-agenda-overriding-header "Appoited Meeting")))))))
  :config
  ;; latex support
  (require 'ox-beamer)
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("ctex" "\\documentclass[11pt]{ctexart}"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-default-packages-alist
		 '("" "color" nil))
    (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))
  ;; capture
  (setq org-capture-templates
	'(("n" "Notes"
	   entry (file+headline wearry/org-agenda-path "Ideas")
	   "* Notes of %? \n \n created on %t \n")
	  ("p" "Project"
	   entry (file+headline wearry/org-agenda-path "PROJECTS") ""))))

;; org-roam configuration
(defconst wearry/bib-lib-paths (list "~/Documents/Zettelkasten/ref-lib.bib"))
(defconst wearry/pdf-lib-paths (list "~/Documents/Zettelkasten/pdf-lib/"))
(defconst wearry/notes-lib-paths (list "~/Documents/Zettelkasten/notes/bib-notes/"))
(defconst wearry/org-roam-notes-path "~/Documents/Zettelkasten/notes")

(use-package org-roam
  :ensure t
  :hook (org-mode . org-roam-db-autosync-mode)
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory wearry/org-roam-notes-path)
  (org-roam-graph-viewer "open")
  (org-roam-graph-executable "neato")
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head "${slug}.org"
			      "#+title: ${title}\n#+created: %U\n#+last-modified: %t\n\n")
	   :immediate-finish t)
	  ("n" "bibliography notes" plain "%?"
	   :if-new (file+head "bib-notes/notes_on_<${citar-title}>_${citar-date}.org"
			      "#+title: Notes on <${citar-title}>\n#+created: %U\n#+last-modified: %t\n\n")
	   :unnarrowed t))
	time-stamp-start "#\\+last-modified:[ \t]*[<\"]")
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
                 (dedicated . t)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.25)
		 (window-parameters . (no-delete-other-windows . t))))
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   (:map org-mode-map
	 (("C-c n i" . org-roam-node-insert)
	  ("C-c n o" . org-id-get-create)
	  ("C-c n g" . org-roam-graph)
	  ("C-c n t" . org-roam-tag-add)
	  ("C-c n l" . org-roam-buffer-toggle)))))

;; config org-citar 
(use-package citar
  :ensure t
  :custom
  (citar-bibliography wearry/bib-lib-paths)
  (citar-library-paths wearry/pdf-lib-paths)
  (citar-notes-paths wearry/notes-lib-paths)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (require 'citar-org)
  (require 'citar-latex)
  (setq citar-file-extensions '("pdf" "org" "md"))
  ;; open PDF files by external PDF viewer
  (add-to-list 'citar-file-open-functions
	       '("pdf" . citar-file-open-external))
  (setq citar-templates
        '((main . "${author editor:35}  ${date year issued:5}  ${title:100}")
          (suffix . "${tags keywords keywords:40}")
          (preview . "${author editor} ${title}, \
${journal publisher container-title collection-title booktitle} \
${volume} (${year issued date}).\n")
          (note . "Notes on ${author editor} ${title}")))
  :bind (("C-c i" . citar-insert-citation)
	 ("C-c o" . citar-open)
	 ("C-c b" . citar-insert-bibtex)
	 ("C-c f" . citar-add-file-to-library)))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package org-ref
  :ensure t
  :after org
  :config
  (setq bibtex-dialect 'biblatex))

(use-package oc
  :config
  (require 'oc-biblatex)
  (require 'oc-csl)
  (require 'citar)
  (setq org-cite-global-bibliography wearry/bib-lib-paths
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((latex biblatex)
                                     (t csl))))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (setq citar-org-roam-capture-template-key "n")
  (setq citar-org-roam-note-title-template "${title}")
  (citar-org-roam-mode))

;; config hugo helper for writting blogs
(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: "))
	   (fname (org-hugo-slug title)))
      (mapconcat #'identity
		 `(,(concat "** TODO " title) ":PROPERTIES:"
		   ,(concat ":EXPORT_FILE_NAME: " fname) ":END:" "\n\n")
		 "\n")))
  (add-to-list 'org-capture-templates
	       ;; It is assumed that below file is present in `org-directory'
	       ;; and that it has a "Blog Drafts" heading. It can even be a
	       ;; symlink pointing to the actual location of all-posts.org!
	       '("h" "Hugo post" entry
		 (file+headline "~/Projects/blog/blogs.org" "Blog Drafts")
		 (function org-hugo-new-subtree-post-capture-template))))

(provide 'init-org)
