;; Don't show the splash screen
(setq inhibit-startup-message t)
;; Turn off some unneeded UI Elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Turn on line numbers in every buffer
(global-display-line-numbers-mode 1)

(use-package modus-themes
:init
(setq modus-themes-hl-line '(accented intense)
      modus-themes-subtle-line-numbers nil
      modus-themes-region '(accented bg-only)
      modus-themes-variable-pitch-ui nil
      modus-themes-fringest 'intense
      modus-themes-diffs nil
      modus-themes-italic-constructs t
      modus-themes-bold-construct t
      modus-themes-prompts '( bold intense italic)
      modus-themes-intense-mouseovers t
      modus-themes-paren-match '(bold intense)
      modus-themes-syntax '(alt-syntax yellow-comments green-strings)
      modus-themes-links '(neutral-underline background)
      modus-themes-mode-line '(moody borderless accented 4 1)
      modus-themes-tabs-accented nil
      modus-themes-completions '((matches . (extrabold intense accented))
                                 (selection . (semibold accented intense))
                                 (popup . (accented)))
      modus-themes-heading '((1 . (rainbow 1.4))
                             (2 . (rainbow 1.3))
                             (3 . (rainbow 1.2))
                             (4 . (rainbow bold 1.1))
                             (t . (rainbow old)))
      modus-themes-org-blocks 'tinted-background
      modus-themes-org-agenta '((header-block . (semibold 1.4))
                                (header-date . (workaholic bold-today 1.2))
                                (event . (accented italic varied))
                                (scheduled . rainbow)
                                (habit . traffic-light))
      modus-themes-markup '(intense background)
      modus-themes-mail-citations 'intensep
      modus-themes-lang-checkers '(background))
  :config


  (load-theme 'modus-vivendi t)
  )

;; Turn on line highlithting for current line
(hl-line-mode 1)
;; Add some margins
(set-fringe-mode 10)

;; Set a font
(set-face-attribute 'default nil :family "PragmataProMonoLiga Nerd Font" :height 150)

;; Proportionally spaced typeface
(set-face-attribute 'variable-pitch nil :family "Fantasque Sans Mono" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "DankMono Nerd Font" :height 1.5)

;; Make escape kill prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Configure pacakge manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

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
  :custom ((doom-modeline-height 20)))

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
  (evil-mode 1)
(use-package eglot :ensure t)

  )

(defun rc/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq org-confirm-babel-evaluate nil)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . rc/org-mode-setup)
   :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t))

(use-package rainbow-mode)
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
		     (list 'org-agenda-mode)))
      (rainbow-mode 1))))
(global-rainbow-mode 1)

(use-package yasnippet
 :config
 (setq yas-snippet-dirs '("~/.doom.d/snippets"))
 (yas-global-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia tsi quelpa-use-package typescript-mode doom-themes hydra evil-collection general rainbow-delimiters which-key counsel rainbow-mode evil lua-mode company modus-themes tree-sitter-langs ivy-rich org-auto-tangle helpful yasnippet all-the-icons doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
