;;; early-init.el --- initialize env  -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (setenv "MACOSX_DEPLOYMENT_TARGET" "27.0"))
