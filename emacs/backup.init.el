;; Don't show the splash screen
(setq inhibit-startup-message t)
;; Turn off some unneeded UI Elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Turn on line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Set a temporary theme
(load-theme 'modus-vivendi t)

;; Turn on line highlithting for current line
(hl-line-mode 1)
;; Add some margins
(set-fringe-mode 10)

;; Set a font
(set-face-attribute 'default nil :font "PragmataProMonoLiga Nerd Font" :height 150)

;; Make escape kill prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Configure pacakge manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;Initalize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 50)))

(use-package all-the-icons
  :if (display-graphic-p))
 
(use-package helpful
  :ensure t)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)


(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)
  )

(defun rc/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . rc/org-mode-setup)
   :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t))

		       
		       







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil helpful all-the-icons doom-modeline ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
