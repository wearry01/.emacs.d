;;; timu-macos-theme-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "timu-macos-theme" "timu-macos-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from timu-macos-theme.el

(autoload 'timu-macos-toggle-dark-light "timu-macos-theme" "\
Toggle between \"dark\" and \"light\" `timu-macos-flavour'." t nil)

(autoload 'timu-macos-toggle-org-colors-intensity "timu-macos-theme" "\
Toggle between intense and non intense colors for `org-mode'.
Customize `timu-macos-org-intense-colors' the to achieve this." t nil)

(autoload 'timu-macos-toggle-mode-line-border "timu-macos-theme" "\
Toggle between borders and no borders for the `mode-line'.
Customize `timu-macos-mode-line-border' the to achieve this." t nil)

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(register-definition-prefixes "timu-macos-theme" '("timu-macos"))

;;;***

;;;### (autoloads nil nil ("timu-macos-theme-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; timu-macos-theme-autoloads.el ends here
