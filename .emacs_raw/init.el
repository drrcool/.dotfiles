;; -*- lexical-binding: t; -*-

;; Garbage Collection
(setq gc-cons-percentage 0.6)

;; Compile warnings
(setq native-comp-async-report-warnings-errors 'silent)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; MISC OPTIMIZATIONS
(setq idle-update-delay 1.0)

;; Disable bidirectional editing
(setq-default bidi-display-reordering 'left-to-right
bidi-paragraph-direction 'left-to-right)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-non-selected-windows nil
fast-but-inprecise-scrolling t
inhibit-compacting-font-caches t)
(menu-bar-mode 0)

(server-start)

;; Revert window changes
(winner-mode 1)

;; INTERACTION
(setq use-short-answers t ;; let y or n work for yes or no
  confirm-kill-emacs 'yes-or-no-p
  initial-major-mode 'org-mode
  initial-scratch-buffer ""
  initial-buffer-choice nil)

;; LINES
(setq-default truncate-lines t
  tab-width 4
  fill-column 80)
(setq line-move-visual t)

;; WINDOWS
(setq frame-resize-pixelwise t
	  ns-pop-up-frames nil
	  window-resize-pixelwise nil
	  split-width-threshold 80)

(use-package paren
  ;; Highlight matching delimiters
  :config
  (setq show-paren-delay 0.1
		show-paren-highlight-openparen t
		show-paren-when-point-inside-paren t
		show-paren-when-point-in-periphery t)
  :init
  (show-paren-mode 1))

;; SCROLLING
(setq scroll-conservatively 101
	  mouse-wheel-ollow-mouse 't
	  mouse-wheel-progressive-speed nil
	  mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 6))
	  mac-redisplay-dont-reset-vscroll t
	  mac-mouse-wheel-smooth-scroll nil)

;; BELL
(setq visible-bell nil
	  ring-bell-function 'ignore)

;;Use system trash
(setq trash-directory "~/.Trash")
(setq delete-by-moving-to-trash t)

;; Follow symlink
(setq vc-follow-symlinks t)

;; BACKUPS
;; Don't generate them
(setq create-lockfiles nil
	  make-backup-files nil
	  )

;; RECENT FILES
(use-package recentf
  :init
  (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
		recentf-filename-handlers (append '(abbreviate-file-name) recentf-filename-handlers))
  )
;; MISC
(setq blink-cursor-interval 0.5)
(blink-cursor-mode 1)
(setq dired-kill-when-opening-new-dired-buffer t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
  (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
  (goto-char (point-max))
  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package general
  :config
(general-evil-setup t)
(general-create-definer rcool/leader-keys
:prefix "SPC")
(general-create-definer rcool/local-leader-keys
:prefix ","
:global-prefix "SPC m"))

(use-package which-key
  :init (which-key-mode))

(use-package evil
  :init
  (evil-mode 1))
  :config
  (setq evil-want-C-i-jump nil
		evil-want-C-d-scroll t
		evil-want-C-u-scroll t
		evil-want-Y-yank-to-eol t
		evil-auto-balance-windows t
		evil-split-window-below t
		evil-split-window-right t
		evil-want-fine-undo t
		evil-want-keybinding nil
		evil-want-fine-undo t
		)
 ;; -- Set cursor colors
(setq evil-emacs-state-cursor '("#649bce" box)
	  evil-normal-state-cursor '("#4000ff" box)
	  evil-operator-state-cursor '("#ebcb8b" hollow)
	  evil-visual-state-cursor '("#677391" box)
	  evil-insert-state-cursor '("#db8114" box)
	  evil-replace-state-cursor '("#ff0000" (hbar))
	  evil-motion-state-cursor '("#ad8beb" box))

(setq mac-command-modifier 'super
  mac-right-command-modifier 'control
  mac-option-modifier 'meta
  mac-right-option-modifier 'meta
  mac-control-modifier 'meta
  mac-right-control-modifier 'control)

(use-package yasnippet
:init
  (yas-global-mode 1)
:config
(setq yas-snippet-dirs
'("~/.dotfiles/.emacs_raw/snippets")))

(use-package rainbow-mode
:init
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
(lambda ()
(when (not (memq major-mode
(list 'org-agenda-mode)))
(rainbow-mode 1))))
(global-rainbow-mode 1)
)

(defvar rcool/default-font-size 200)
  (set-face-attribute 'default nil :font "Spleen32x64 Nerd Font" :height rcool/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "Spleen32x64 Nerd Font" :height 210)
  (set-face-attribute 'variable-pitch nil :font "Spleen32x64 Nerd Font" :height 220 :weight 'regular)

(use-package doom-themes
  :defer t
  :init
  (load-theme 'doom-Iosvkem t)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun rcool/org-babel-tangle-config ()
(let ((org-confirm-babel-evaluate nil))
(org-babel-tangle)))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rcool/org-babel-tangle-config)))

(use-package company
  :diminish
  :general
  (general-define-key :keymaps 'company-active-map
					  "C-j" 'company-select-next
					  "C-k" 'company-select-previous)

  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2
		company-tooltip-limit 14
		company-tooltip-align-annotations t
		company-require-match 'never
		company-global-modes '(not erc-mode message-mode help-mode gud-mode)
		company-frontends
		'(company-pseudo-tooltip-frontend
		   company-echo-metadata-frontend)
		company-backends '(company-capf company-files company-keywords)
		company-auto-complete nil
		company-auto-complete-chars nil
		company-dabbrev-other-buffers nil
		company-dabbrev-ignore-case nil
		company-dabbrev-downcase nil)

  :config
  (setq company-idle-delay 0.35)
  :custom-face
  (company-tooltip ((t (:family "Spleen32x64 Nerd Font")))))

(setq display-time-default-load-average nil)
(line-number-mode)
(column-number-mode)
(display-time-mode)
(size-indication-mode 0)

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package doom-modeline
  :init
  (doom-modeline-mode)

  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
		doom-modeline-enable-word-count nil
		doom-modeline-buffer-encoding nil
		doom-modeline-icon t
		doom-modeline-modal-icon t
		doom-modeline-major-mode-icon t
		doom-modeline-major-mode-color-icon t
		doom-modeline-bar-width 3
		doom-modeline-height 28))

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil)
  (add-hook 'org-agenda-mode-hook
			#'(lambda () (setq local nobreak-char-display nil)))
  :init
  (org-super-agenda-mode))

(use-package centered-cursor-mode :diminish)

(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " "
		org-superstart-special-todo-items t
		org-superstar-todo-bullet-alist '(("TODO" . 9744)
										  ("INPROG" . 9744)
									  ("NEXT" . 9744)
										  ("READ" . 9744)
										  ("CANCELLED" . 9745)
										  ("DONE" . 9745)
										  ))
  :hook (org-mode . org-superstar-mode)
  )

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   org-modern-star '( "⌾" "✸" "◈" "◇")
   org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil))

(use-package evil-org
  :diminish
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
			(lambda () (evil-org-set-key-theme))))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(use-package org-gcal
  :defer t
  :config
  (setq org-gcal-down-days '20
		org-gcal-up-days '10
		org-gcal-recurring-events-mode 'top-level
		org-gcal-remove-api-cancelled-events t))

(use-package org-appear
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
		org-appear-autoemphasis t
		org-appear-autolinks nil
		org-appear-autosubmarkers t))

(use-package ox-reveal
  :defer 5)

(setq org-modules '(org-habit))
(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package org-ql
  :defer t
  :general
  (general-define-key :keymaps 'org-ql-view-map
					  "q" 'kill-buffer-and-window)
  )

(use-package org-tree-slide
:defer t
:config
(setq org-tree-slide-slide-in-effect nil
 org-tree-slide-skip-outline-level 3))

(use-package valign :defer t)

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
                   :files (:defaults "extensions/*"))

  :init
  (setq org-roam-v2-ack t)

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  (org-roam-db-autosync-mode)

  :custom
  (org-roam-directory (file-truename "~/org"))
  (org-roam-dailies-directory "roam/daily/")
  (org-roam-completion-everywhere t)
 )

(setq org-special-ctrl-a/e t)

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "t" 'org-todo
  "<return>" 'org-open-at-point-global
  "K" 'org-shiftup
  "J" 'org-shiftdown
  )
(general-def
  :states 'insert
  :keymaps 'org-mode-map
  "C-o" 'evil-org-open-above
  "M-<up>" 'org-shiftup
  "M-<down>" 'org-shiftdown
  "<" 'org-shiftleft
  ">" 'org-shiftright
  )

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "M-[" 'org-metaleft
  "M-]" 'org-metaright
  )

;; Org-src
(general-def
  :prefix ","
:states 'normal
  :keymaps 'org-src-mode-map
  "b" '(nil :wk "Org SRC")
  "bk" '(org-edit-src-abort :wk "Abort")
  "bc" '(org-edit-src-exit :wk "Accept & Exit")
  )

(general-def
  :keymaps 'org-src-mode-map
  "C-c C-c" 'org-edit-src-exit)

(defun rcool/org-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (centered-cursor-mode)
  (smartparens-mode 0)
  (hl-prog-extra-mode 0)
  (setq-local line-spacing 3)
  (valign-mode)
  )

(defun rcool/prettify-symbols-setup ()
  ;; checkboxes
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;; (push '("[X]" . "☒" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)

  ;; org-babel
  (push '("#+BEGIN_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+END_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+begin_src" . ?≫) prettify-symbols-alist)
  (push '("#+end_src" . ?≫) prettify-symbols-alist)

  (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
  (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)

  ;; (push '("#+BEGIN_SRC python" . ) prettify-symbols-alist) ;; This is the Python symbol. Comes up weird for some reason
  (push '("#+RESULTS:" . ?≚ ) prettify-symbols-alist)

  ;; drawers
  (push '(":PROPERTIES:" . ?) prettify-symbols-alist)

  ;; tags
  ;; (push '(":Misc:" . "" ) prettify-symbols-alist)

  (prettify-symbols-mode))

(use-package org
  :hook (org-mode . rcool/org-setup)
  :hook (org-mode . rcool/prettify-symbols-setup)
  :hook (org-capture-mode . evil-insert-state)
  :diminish org-indent-mode
  :diminish visual-line-mode
  )

(use-package org
  :config

  (setq org-cycle-separator-lines 1
		org-catch-invisible-edits 'show-and-error
		org-src-tab-acts-natively t)

  ;; M-RET can split lines items and tables but not headlines
  (setq org-M-RET-may-split-line '((headline) (item . t) (default)))
  (setq org-loop-over-headlines-in-active-region nil)

  ;;Open Links to other org files in the same frame
  (setq org-link-frame-setup '((file . find-file)))

  ;; Log when tasks are done
  (setq org-log-done t
		org-log-into-drawer t)

  ;; Automatically change bullet point when indenting
  (setq org-list-demote-modify-bullet
		'(("+" . "*") ("*" . "-") ("-" . "+")))

  (defun rcool/post-org-goto ()
	(let ((current-prefix-arg '(4)))
	  (call-interactively 'org-reveal))
	(org-cycle))

  (advice-add 'counsel-org-togo :after #'rcool/post-org-goto)
  (advice-add 'org-agenda-goto :after #'rcool/post-org-goto)
  (advice-add 'org-agenda-switch-to :after #'rcool/post-org-goto)

  )

(use-package org
  :config
  (setq org-tags-column -1)
  )

(use-package org
:config
(setq org-todo-keywords '((type
                           "TODO(t)" "WAITING(h)" "INPROG(i)"
                           "READ(r)" "PROJ(p)" "DONE(d)" "CANCELLED(C@)")))
(setq org-todo-keyword-faces
      '(("TODO" :inherit (region org-todo) :foreground "DarkOrange1" :weight bold)
        ("WAITING" :inherit (org-todo region) :foreground "DarkOrange1" :weight bold)
        ("INPROG" :inherit (org-todo region) :foreground "MediumPurple2" :weight bold)
        ("READ" :inherit (org-todo region) :foreground "orange3" :weight bold)
        ("PROJ" :inherit (org-todo region) :foreground "blue3" :weight bold)
        ("DONE" . "SeaGreen4")
        ("CANCELLED" . "SeaGreen4")))
)

(use-package org
:config
 (setq org-lowest-priority ?F)
 (setq org-default-priority ?E)

 (setq org-priority-faces
       '((65 . "red2")
       (66 . "Gold1")
       (67 . "DarkOrange1")
       (68 . "PaleTurquoise3")
       (69 . "DarkSlateGrey4")
       (70 . "PaleTurquoise4")))
)

(use-package org
  :config

  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (python . t)
      (shell . t)
      (gnuplot . t)
      (emacs-lisp . t)
      (js . t)
      (sql . t)
      (calc . t)
      (lua . t)))

  (use-package gnuplot :defer t)


  ;; Don't prompt before running code
  (setq org-confirm-babel-evaluate nil
        python-shell-completion-native-enable nil)

  ;; How to open the src buffer
  (setq org-src-window-setup 'current-window)
  )

(use-package smartparens
  :diminish
  :defer 1
  :config
  (require 'smartparens-config)
  (setq sp-max-prefix-length 25
        sp-max-pair-length 4
        sp-highlight-pair-overlay nil
        sp-highlihgt-wrap-overlay nil
        sp-highlihgt-wrap-tag-overlap nil)

  (with-eval-after-load 'evil
    (setq sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-pair-overlay-keymap (make-sparse-keymap)))

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'" nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("|" "SPC")))
  (smartparens-global-mode t))

(use-package hl-prog-extra
:commands (hl-prog-extra-mode)
  :config
  (setq hl-prog-extra-list
      (list
       '("\\<\\(TODO\\|NOTE\\)\\(([^)+]+)\\)?" 0 comment
         (:weight bold :inherit diff-removed))
       ;; Match TKs in quotation marks (hl-prog-extra sees them as strings)
       '("\\(TK\\)+" 0 string '(:weight bold :inherit font-lock-warning-face))
       ;; Match TKs not in quotation marks
       '("\\(TK\\)+" 0 nil '(:weight bold :inherit font-lock-warning-face))))
(global-hl-prog-extra-mode))
