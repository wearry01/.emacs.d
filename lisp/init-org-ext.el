;;; lisp/init-org-ext.el --- Org Extensions Configuration
;;; org-roam, citar, ox-hugo

;; config local variables for paths
(defvar wearry/org-roam-notes-path "~/Documents/Zettelkasten/roam-notes/")
(defvar wearry/bib-lib-paths (list "~/Documents/Zettelkasten/ref-lib.bib"))
(defvar wearry/pdf-lib-paths (list "~/Documents/Zettelkasten/pdf-lib/"))
(defvar wearry/notes-lib-paths (list "~/Documents/Zettelkasten/roam-notes/bib-notes/"))

(defvar wearry/org-journal-dir "~/Documents/Zettelkasten/journals/"
  "Weekly journal directory")

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
	       `("w" "Weekly Journal" entry
		 (file ,(expand-file-name
			 (format-time-string "%Y/%Y-W%V.org")
			 wearry/org-journal-dir))
		 "* Week Journal %<%Y-W%V>\n** Time Usage\n%?** Highlights\n** To Do Next Week\n"
		 :jump-to-captured t)))

(defvar wearry/org-thoughts-dir
  (expand-file-name "thoughts/" wearry/org-roam-notes-path))

(defun wearry/open-citar-bib ()
  (interactive)
  (find-file (car wearry/bib-lib-paths)))

;; org-roam configuration
(use-package org-roam
  :ensure t
  :defer t
  :init
  :custom
  (org-roam-directory wearry/org-roam-notes-path)
  (org-roam-dailies-directory wearry/org-thoughts-dir)

  (org-roam-graph-viewer "open")
  (org-roam-graph-executable "neato")
  (org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n d" . org-roam-dailies-goto-date)
   ("C-c n r" . org-roam-node-random)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n g" . org-roam-graph)
   ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
                 (dedicated . t)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.3)
		 (window-parameters . (no-delete-other-windows . t))))
  (setq org-roam-capture-templates
	'(;; default entries for roam nodes
	  ("d" "default" plain "\n%?"
	   :if-new (file+head
		    "${slug}.org"
		    "#+title: ${title}\n#+created: %U\n#+last-modified: %t\n")
	   :immediate-finish t)
	  ;; bibliography notes
	  ("n" "bibliography notes" plain "\n%?"
	   :if-new (file+head
		    "bib-notes/notes_on_<${citar-title}>_${citar-date}.org"
		    "#+title: Notes on <${citar-title}>\n\
#+created: %U\n\
#+last-modified: %t\n")
	   :unnarrowed t))
	time-stamp-start "#\\+last-modified:[ \t]*[<\"]")
  (setq org-roam-dailies-capture-templates
	'(;; daily thoughts/records
	  ("t" "thought" plain "\n%?"
	   :if-new (file+head
		    "%<%Y-%m-%d>.org"
		    "#+title: %<%Y-%m-%d> Record\n#+created: %U\n#+last-modified: %t\n")
	   :unnarrowed t))
	time-stamp-start "#\\+last-modified:[ \t]*[<\"]"))

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
	 ("C-c b" . wearry/open-citar-bib)
	 ("C-c f" . citar-add-file-to-library)))

(use-package oc
  :config
  (require 'oc-biblatex)
  (require 'oc-csl)
  (require 'citar)
  (setq org-cite-global-bibliography wearry/bib-lib-paths
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((latex biblatex) (t csl))))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-capture-template-key "n")
  (setq citar-org-roam-note-title-template "${title}"))

;; config hugo helper for writting blogs
(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

;; Hugo capture helper
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

(provide 'init-org-ext)
