;;; early-init.el --- Early init file -*- lexical-binding: t; no-byte-compile: t -*-
;; Defer garbage collection further back in the startup processs
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)

;; Keep emacs from running package initilization
(setq pacakge-enable-at-startup nil)
;; Do not load from package cache
(setq package-quickstart nil)

;; Get rid of unstyled emacs
(push '(menu-bar-lines  . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the frame is taxing. Keep it small and ignore fonts for a bit
(setq frame-inhibit-implied-resize t)

;; Remove gui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; prevent undwanted runtime buils
(setq comp-deferred-compilation nil)

(provide 'early-init)
;;; early-init.el ends here
