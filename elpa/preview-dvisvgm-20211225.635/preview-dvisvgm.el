;;; preview-dvisvgm.el --- SVG output for LaTeX preview  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: tex
;; Package-Version: 20211221.1744
;; Package-Commit: 72ee787acdbf0b7767ef9a8d26a74c7225eec4d3
;; Version: 1.0.0
;; URL: https://github.com/TobiasZawada/preview-dvisvgm
;; Package-Requires: ((emacs "27.1") (auctex "13.0.12"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate SVG images for the preview-latex package.
;;
;; Installation:
;; This package requires the preview-latex package
;; (see https://www.gnu.org/software/auctex/manual/preview-latex.html).
;; It is tested with version 13.0.12 of the preview-latex package.
;; Furthermore, you need a working LaTeX installation and
;; the programm `dvisvgm' installed on your computer (see https://dvisvgm.de/).
;;
;; Put preview-dvisvgm.el somewhere in your `load-path' and require it
;; your init file (e.g., "~/.emacs.d/init.el") with
;;
;; (require 'preview-dvisvgm)
;;
;; Note, that this package adds an entry for dvisvgm to preview-image-creators.
;; (Currently, this appears to be a customization which it isn't.  This is a known dept.)
;;
;; Usage:
;; If you like preview-latex to generate svg images customize option
;; `preview-image-type' to "dvisvgm".
;;
;;; Code:

(require 'preview)
(require 'cl-lib)
(require 'face-remap)

;; Solve the chicken-or-egg problem with "preview" and "preview-dvisvgm":
;; The following (require 'preview-dvisvgm) does
;; not hurt, since in the case that "preview" is already loaded, the
;; above (require 'preview) does nothing.
;;;###autoload (with-eval-after-load 'preview (require 'preview-dvisvgm))

(defcustom preview-dvisvgm-debug 0
  "Verbosity level for debugging of preview-dvisvgm."
  :type 'integer
  :group 'preview-gs)

(defun preview-dvisvgm-variable-standard-value (symbol)
  "Return standard value of variable SYMBOL."
  (let ((container (get symbol 'standard-value)))
    (cl-assert (consp container) "%s does not have a standard value")
    (eval (car container))))

(defun preview-dvisvgm-set-variable-standard-value (symbol value)
  "Set standard value of variable SYMBOL to VALUE."
  (let ((standard (preview-dvisvgm-variable-standard-value symbol)))
    (when (equal (symbol-value symbol) standard)
      (set symbol value))
    (put symbol 'standard-value (list
                 (list
                  'quote
                  value)))))

(gv-define-simple-setter preview-dvisvgm-variable-standard-value preview-dvisvgm-set-variable-standard-value)

(cl-pushnew '(dvisvgm (open preview-gs-open preview-dvisvgm-process-setup)
		      (place preview-gs-place)
		      (close preview-dvisvgm-close))
	    (preview-dvisvgm-variable-standard-value 'preview-image-creators)
	    :test #'equal)

(put 'preview-image-type 'custom-type
     (append '(choice)
             (mapcar (lambda (symbol) (list 'const (car symbol)))
                     preview-image-creators)
             '((symbol :tag "Other"))))

(cl-pushnew '(dvisvgm png "-sDEVICE=png16m")
	    (preview-dvisvgm-variable-standard-value 'preview-gs-image-type-alist)
	    :test #'equal)

(defcustom preview-dvisvgm-command
  "dvisvgm --no-fonts %d --page=- --output=\"%m/prev%%3p.svg\""
  "Command used for converting to separate svg images.

Note, most SVG renderes do not support <font>...</font>-elements
generated by dvisvgm.

You might specify options for converting to other image types,
but then you'll need to adapt `preview-dvisvgm-image-type'."
  :group 'preview-latex
  :type 'string)

(defcustom preview-dvisvgm-pdf-command
  "dvisvgm --pdf --no-fonts %(O?pdf) --page=- --output=\"%m/prev%%3p.svg\""
  "Command used for converting the generated pdf file to separate svg images.

Note, most SVG renderes do not support <font>...</font>-elements
generated by dvisvgm.

You might specify options for converting to other image types,
but then you'll need to adapt `preview-dvisvgm-image-type'."
  :group 'preview-latex
  :type 'string)

(defcustom preview-dvisvgm-image-type
  'svg
  "Image type that dvisvgm produces.

You'll need to change `preview-dvisvgm-command'
and `preview-dvisvgm-pdf-command' too,
if you customize this."
  :group 'preview-latex
  :type '(choice (const svg)
                 (const gif)
                 (symbol :tag "Other" :value png)))

(defun preview-dvisvgm-process-setup ()
  "Set up dvisvgm process for conversion."
  (when (> preview-dvisvgm-debug 0)
    (message "Running `preview-dvisvgm-process-setup'"))
  (setq preview-gs-command-line (append
                                 preview-gs-command-line
                                 (list (preview-gs-resolution
                                        (preview-hook-enquiry preview-scale)
                                        (car preview-resolution)
                                        (cdr preview-resolution)))))
  (unless (preview-supports-image-type preview-dvisvgm-image-type)
    (error "Setting \"%s\" for `preview-dvisvgm-image-type'  unsupported by this Emacs"
           preview-dvisvgm-image-type))
  (let ((process (preview-dvisvgm-start)))
    (setq TeX-sentinel-function #'preview-dvisvgm-sentinel)
    (list process (current-buffer) TeX-active-tempdir t
          preview-dvisvgm-image-type)))

(defun preview-dvisvgm-start ()
  "Start a DviSvgm process.."
  (when (> preview-dvisvgm-debug 0)
    (message "Running `preview-dvisvgm-start'."))
  (let* (;; (file preview-gs-file)
         tempdir
         (scale (* (/ (preview-hook-enquiry preview-scale)
                      (preview-get-magnification))
		   (with-current-buffer TeX-command-buffer
		     (if text-scale-mode
		       (expt text-scale-mode-step text-scale-mode-amount)
		     1.0))))
         (scale-str  (format " --scale=%g " scale))
         (command (with-current-buffer TeX-command-buffer
                    (prog1
                        (concat (TeX-command-expand
				 (if (or TeX-PDF-mode preview-parsed-pdfoutput)
				     preview-dvisvgm-pdf-command
				     preview-dvisvgm-command))
                                " " scale-str)
                      (setq tempdir TeX-active-tempdir))))
         (name "Preview-DviSVGM"))
    (setq TeX-active-tempdir tempdir)
    (goto-char (point-max))
    (insert-before-markers "Running `" name "' with ``" command "''\n")
    (setq mode-name name)
    (setq TeX-sentinel-function
          (lambda (_process name) (message "%s: done." name)))
    (if TeX-process-asynchronous
        (let ((process (start-process name (current-buffer) TeX-shell
                                      TeX-shell-command-option
                                      command)))
          (if TeX-after-start-process-function
              (funcall TeX-after-start-process-function process))
          (TeX-command-mode-line process)
          (set-process-filter process #'TeX-command-filter)
          (set-process-sentinel process #'TeX-command-sentinel)
          (set-marker (process-mark process) (point-max))
          (push process compilation-in-progress)
          (sit-for 0)
          process)
      (setq mode-line-process ": run")
      (force-mode-line-update)
      (call-process TeX-shell nil (current-buffer) nil
                    TeX-shell-command-option
                    command))))

(defun preview-dvisvgm-place-all ()
  "Place all images dvisvgm has created, if any.
Deletes the dvi file when finished."
  (when (> preview-dvisvgm-debug 0)
    (message "Running `preview-dvisvgm-place-all'."))
  (let (filename queued oldfiles snippet)
    (dolist (ov (prog1 preview-gs-queue (setq preview-gs-queue nil)))
      (when (and (setq queued (overlay-get ov 'queued))
                 (setq snippet (aref (overlay-get ov 'queued) 2))
                 (setq filename (preview-make-filename
                                 (format "prev%03d.%s"
                                         snippet preview-dvisvgm-image-type)
                                 TeX-active-tempdir)))
        (if (file-exists-p (car filename))
            (progn
	      (when (>= preview-dvisvgm-debug 2)
		(message "Found image \"%s\"." filename))
              (overlay-put ov 'filenames (list filename))
              (preview-replace-active-icon
               ov
               (preview-create-icon (car filename)
                                    preview-dvisvgm-image-type
                                    (preview-ascent-from-bb
                                     (aref queued 0))
                                    (aref preview-colors 2)))
              (overlay-put ov 'queued nil))
	  (when (>= preview-dvisvgm-debug 2)
		(message "Image file \"%s\" not found." filename))
          (push filename oldfiles)
          (overlay-put ov 'filenames nil)
          (push ov preview-gs-queue))))
    (if (setq preview-gs-queue (nreverse preview-gs-queue))
        (progn
          (preview-dvisvgm-start)
          (setq TeX-sentinel-function (lambda (process command)
                                        (preview-dvisvgm-sentinel
                                         process
                                         command
                                         t)))
          (dolist (ov preview-gs-queue)
            (setq snippet (aref (overlay-get ov 'queued) 2))
            (overlay-put ov 'filenames
                         (list
                          (preview-make-filename
                           (or preview-ps-file
                               (format "preview.%03d" snippet))
                           TeX-active-tempdir))))
          (while (setq filename (pop oldfiles))
            (condition-case nil
                (preview-delete-file filename)
              (file-error nil))))
      (condition-case nil
          (let ((gsfile preview-gs-file))
            (delete-file
             (with-current-buffer TeX-command-buffer
               (funcall (car gsfile) "dvi" t))))
        (file-error nil)))))

(defalias 'preview-dvisvgm-abort #'preview-dvips-abort)

(defun preview-dvisvgm-sentinel (process command &optional placeall)
  "Sentinel function for indirect rendering DviSVGM process.
The usual PROCESS and COMMAND arguments for
`TeX-sentinel-function' apply.  Places all snippets if PLACEALL is set."
  (when (> preview-dvisvgm-debug 0)
    (message "Running (preview-dvisvgm-sentinel %s %s %s)." process command placeall))
  (condition-case err
      (let ((status (process-status process)))
        (cond ((eq status 'exit)
               (delete-process process)
               (setq TeX-sentinel-function nil)
               (when placeall
                 (preview-dvisvgm-place-all)))
              ((eq status 'signal)
               (delete-process process)
               (preview-dvisvgm-abort)
	       )))
    (error (preview-log-error err "DviPNG sentinel" process)))
  (preview-reraise-error process))

(defun preview-dvisvgm-close (process closedata)
  "Clean up after PROCESS and set up queue accumulated in CLOSEDATA."
  (when (> preview-dvisvgm-debug 0)
    (message "Running `preview-dvisvgm-close'."))
  (setq preview-gs-queue (nconc preview-gs-queue closedata))
  (if process
      (if preview-gs-queue
          (if TeX-process-asynchronous
              (if (and (eq (process-status process) 'exit)
                       (null TeX-sentinel-function))
                  ;; Process has already finished and run sentinel
                  (preview-dvisvgm-place-all)
                (setq TeX-sentinel-function (lambda (process command)
                                              (preview-dvisvgm-sentinel
                                               process
                                               command
                                               t))))
            (TeX-synchronous-sentinel "Preview-DviSVGM" (cdr preview-gs-file)
                                      process))
        ;; pathological case: no previews although we sure thought so.
        (delete-process process)
        (unless (eq (process-status process) 'signal)
          (preview-dvipng-abort)))))

(provide 'preview-dvisvgm)
;;; preview-dvisvgm.el ends here
