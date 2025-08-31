;;; lisp/init-org-ext.el --- Org Extensions Configuration
;;; org-roam, citar, ox-hugo

(defvar wearry/org-roam-notes-path "~/Documents/Zettelkasten/notes")
(defvar wearry/bib-lib-paths (list "~/Documents/Zettelkasten/ref-lib.bib"))
(defvar wearry/pdf-lib-paths (list "~/Documents/Zettelkasten/pdf-lib/"))
(defvar wearry/notes-lib-paths (list "~/Documents/Zettelkasten/notes/bib-notes/"))

;; org-roam configuration
(use-package org-roam
  :ensure t
  :defer t
  :init
  :custom
  (org-roam-directory wearry/org-roam-notes-path)
  (org-roam-graph-viewer "open")
  (org-roam-graph-executable "neato")
  (org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head
		    "${slug}.org"
		    "#+title: ${title}\n#+created: %U\n#+last-modified: %t\n\n")
	   :immediate-finish t)
	  ("n" "bibliography notes" plain "%?"
	   :if-new (file+head
		    "bib-notes/notes_on_<${citar-title}>_${citar-date}.org"
		    "#+title: Notes on <${citar-title}>\n#+created: %U\n#+last-modified: %t\n\n")
	   :unnarrowed t))
	time-stamp-start "#\\+last-modified:[ \t]*[<\"]")
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
                 (dedicated . t)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.3)
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
        org-cite-export-processors '((latex biblatex) (t csl))))

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

(defun wearry/org-insert-log-into-archive (archive-file path drawer line)
  "Insert LINE into ARCHIVE-FILE under HEADING with LEVEL,
inside LOGBOOK. Adds an :ARCHIVE: tag to the heading if
not already present."
  (with-current-buffer (find-file-noselect archive-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     ;; create headline level by level
     (let ((level 1))
       (dolist (h path)
         (let ((stars (make-string level ?*)))
           (unless (re-search-forward
                    (format "^%s %s" stars (regexp-quote h)) nil t)
             (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             (insert stars " " h (if (= level 1) " :ARCHIVE:" "") "\n")))
         (org-back-to-heading t)
         (setq level (+ 1 level))))
     (org-back-to-heading t)
     ;; locate LOGBOOK
     (if (re-search-forward drawer (save-excursion (org-end-of-subtree t)) t)
	 (forward-line 1)
       ;; create a new if not exist
       (org-end-of-meta-data t)
       (insert drawer "\n:END:\n")
       (forward-line -1))
     (insert line)
     ;; sorting LOGBOOK entries
     (forward-line -1)
     (save-excursion
       (let ((beg (line-beginning-position)) end)
	 (re-search-forward ":END:" nil t) ;; back to :END:
	 (forward-line -1)
	 (setq end (line-end-position))
	 (sort-lines t beg end)))
     (save-buffer))))

(defun wearry/org-archive-old-log-entries-by-drawer (months drawer)
  (let* ((cutoff (time-subtract (current-time)
				(days-to-time (* 30 months))))
	 (archive-dir (expand-file-name "archive/"
					(file-name-directory (buffer-file-name))))
	 (archive-file (expand-file-name
			(concat (file-name-base (buffer-file-name)) "-log-archive.org")
			archive-dir)))
    ;; 确保子文件夹存在
    (unless (file-directory-p archive-dir)
      (make-directory archive-dir t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward drawer nil t)
        (let ((log-start (match-beginning 0))
              path)
          ;; save title info
          (save-excursion
            (org-back-to-heading t)
            (setq path (append (org-get-outline-path t)
                               (list (org-get-heading t t t t)))))
          ;; Match line by line until meeting :END:
	  (while (and (not (looking-at ":END:"))
		      (not (eobp)))
	    (let* ((line (thing-at-point 'line t))
		   (date (and (string-match
			       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
			       line)
			      (match-string 1 line))))
	      (cond
	       ((and date (time-less-p (org-time-string-to-time date) cutoff))
		(wearry/org-insert-log-into-archive archive-file path drawer line)
		;; delete current line including RET
		(delete-region (line-beginning-position)
			       (+ 1 (line-end-position))))
	       (t (unless (looking-at ":END:")
		    (forward-line 1)))))))))
    (save-buffer)))

(defun wearry/org-archive-old-log-entries (&optional months drawers)
  "Archive LOGBOOK entries older than MONTHS (default 1)
into a corresponding headline in an archive file.
Keeps tasks themselves, only archives old log lines.
Archive headlines get an :ARCHIVE: tag."
  (interactive "P")
  (let* ((months (or months 1))
	 (drawers (or drawers (list ":LOGBOOK:" ":LOGSTATE:"))))
    (dolist (drawer drawers)
      (wearry/org-archive-old-log-entries-by-drawer months drawer))))

(provide 'init-org-ext)
