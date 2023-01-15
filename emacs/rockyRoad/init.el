;;; init.el --- Personal config -*- legical-bindig: t; no-byte-compile: t; -*-
;;; Commentary:
;;;
;;; Code:
;; file-name-handler-alist is called often, we optimize a little to make it faster
(unless (daemonp)
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (defun doom-reset-file-handler-alist-h ()
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-hander-alist handler))
    (setq file-name-handler--alist doom--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)
(add-hook 'after-init-hook '(lambda() (setq gc-cons-threshold 16777216
                                       gc-cons-percentage 0.1))))

(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))


;; Load packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((file-name-handler-alist nil)
      (gc-cons-threshold 100000000))
  (require 'init-core)
;;; init.el ends here
  )
