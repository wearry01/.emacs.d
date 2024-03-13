;; Init Org Mode

(defconst wearry/bib-lib-paths (list "~/Documents/Zettelkasten/ref-lib.bib"))
(defconst wearry/pdf-lib-paths (list "~/Documents/Zettelkasten/pdf-lib/"))
(defconst wearry/org-roam-notes-path "~/Documents/Zettelkasten/notes")


(use-package citar
  :ensure t
  :custom
  (citar-bibliography wearry/bib-lib-paths)
  (citar-library-paths wearry/pdf-lib-paths)
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
        '((main . "${author editor:35}     ${date year issued:4}     ${title:65}")
          (suffix . "  ${tags keywords keywords:40}")
          (preview . "${author editor} ${title}, \
${journal publisher container-title collection-title booktitle} \
${volume} (${year issued date}).\n")
          (note . "Notes on ${author editor} ${title}")))
  :bind (("C-c i" . citar-insert-citation)
	 ("C-c o" . citar-open)
	 ("C-c b" . citar-insert-bibtex)
	 ("C-c a" . citar-add-file-to-library)))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package org
  :ensure org-contrib
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture))
  :config
  (require 'org-checklist)
  (setq org-log-done t
	org-log-into-drawer t
	org-startup-indented t
	org-highlight-latex-and-related '(native script entities))
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
    (setq org-latex-compiler "xelatex")))

(use-package org-ref
  :ensure t
  :after org
  :config
  (setq bibtex-dialect 'biblatex))

(use-package org-roam
  :ensure t
  :hook (org-mode . org-roam-db-autosync-mode)
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom (org-roam-directory wearry/org-roam-notes-path)
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
	  ("C-c n a" . org-roam-alias-add)
	  ("C-c n l" . org-roam-buffer-toggle)))))

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

;; Configure Hugo Post for Writing Blogs

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
	   (fname (org-hugo-slug title)))
      (mapconcat #'identity
		 `(,(concat "** TODO " title)
		   ":PROPERTIES:"
		   ,(concat ":EXPORT_FILE_NAME: " fname)
		   ":END:" "\n\n") ;Place the cursor here finally
		 "\n")))
  (add-to-list 'org-capture-templates
	       '("h" "Hugo post" entry
		 ;; It is assumed that below file is present in `org-directory'
		 ;; and that it has a "Blog Ideas" heading. It can even be a
		 ;; symlink pointing to the actual location of all-posts.org!
		 (file+headline "~/Projects/blog/blogs.org" "Blog Drafts")
		 (function org-hugo-new-subtree-post-capture-template))))

(provide 'init-org)
