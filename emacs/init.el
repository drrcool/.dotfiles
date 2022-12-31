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

;;Initalize use-package
(unless (package-installed-p 'use-package)
(package-install 'use-package))

(require 'use-package)

;;-*- lexical-binding: t; -*-
;; Don't show the splash screen
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)
;; Tune garbage collection
(setq gc-cons-threshold (* 50 1000 1000))

;; Turn off some unneeded UI Elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)

(setq delete-by-moving-to-trash t
trash-directory "~/.local/share/Trash/files/")

(use-package beacon
:straight (beacon :fetcher github :repo "Malabarba/beacon")
  :ensure t
  :config
  (beacon-mode +1))

(setq org-confirm-babel-evaluate nil)

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(load (expand-file-name "rc-funcs.el" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq vc-follow-symlinks t)
(setq vc-suppress-confirm t)

;; Turn on line numbers in every buffer
(global-display-line-numbers-mode 1)
;; With vim bindings, relatives let us use j and k better
(setq display-line-numbers-type 'relative)

(setq create-lockfiles nil)

;; Turn on line highlithting for current line
(hl-line-mode 1)
;; Add some margins
(set-fringe-mode 10)

;; Make escape kill prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq mac-command-modifier       'meta
      mac-option-modifier        'meta
      mac-control-modifier       'control
      mac-right-option-modifier     'meta
      mac-right-control-modifier  'control)

(use-package gcmh
  :diminish gcmh-mode
  :straight (gcmh-mode :host github :repo "akirak/gcmh")
  :config
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(winner-mode +1)
(setq display-buffer-base-action
  '((display-buffer-reuse-window
     display-buffer-reuse-mode-window
     display-buffer-same-window
     display-buffer-in-previous-window)))

(use-package doom-modeline
  :straight (doom-modeline :repo "seagle0128/doom-modeline" :fetcher github)
      :ensure t
      :init (doom-modeline-mode 1)
      :config
      (setq doom-modeline-hud nil)
      (setq doom-modeline-minor-modes nil)
      (setq rc-doom-modeline-text-height 200)
      (setq doom-modeline-height 35)
      )
    (use-package all-the-icons
:straight (all-the-icons
 :repo "domtronn/all-the-icons.el"
 :fetcher github
 :files (:defaults "data"))
      :ensure t
      :if (display-graphic-p))

(use-package modus-themes
:ensure t
:straight (modus-themes :fetcher sourcehut :repo "protesilaos/modus-themes")
:init
(setq modus-themes-hl-line '(intense)
modus-themes-subtle-line-numbers nil
modus-themes-region '(no-extend bg-only)
modus-themes-variable-pitch-ui nil
modus-themes-fringes 'intense
modus-themes-diffs nil
modus-themes-italic-constructs t
modus-themes-bold-constructs  t
modus-themes-prompts '( bold intense italic)
modus-themes-intense-mouseovers t
modus-themes-paren-match '(bold intense)
modus-themes-syntax '( yellow-comments)
modus-themes-links '(neutral-underline background)
modus-themes-mode-line '(moody borderless accented 4 1)
modus-themes-headings
 '((1 . (variable-pitch 1.1 rainbow))
                       (2 . (regular))
                       (3 . (regular))
                       (4 . (regular))
                       (t . (rainbow)))
modus-themes-org-blocks `gray-background
 ))

(use-package mixed-pitch
  :ensure t
  :straight (mixed-pitch :fetcher gitlab :repo "jabranham/mixed-pitch")
  :defer t
  :config
  (setq mixed-pitch-set-height nil)
  (dolist (face '(org-date org-priority org-tag org-special-keyword))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
 )

(use-package doom-themes
  :straight t
  :after mixed-pitch
    :ensure t
    :config
    (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
    (load-theme 'doom-1337 t)

    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (setq doom-themes-treemacs-theme "doom-atom")
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
)

  (load-theme 'doom-Iosvkem t)

(use-package kaolin-themes 
   :straight (kaolin-themes
:fetcher github
:repo "ogdenwebb/emacs-kaolin-themes"
:old-names (kaolin-theme)
:files (:defaults "themes/*.el"))
     :ensure t
     :config
     (setq kaolin-themes-modeline-border nil)
 )

(use-package ef-themes
  :ensure t
  :straight (ef-themes :host github :repo "protesilaos/ef-themes")
  :init
  (setq ef-themes-headings
	(quote ((1 . (variable-pitch 1.1))
		(2 . (regular))
		(3 . (regular))
		(4 . (regular))))))

(set-face-attribute 'default nil :family "PragmataProMonoLiga Nerd Font" :height 200)

 ;; Proportionally spaced typeface
 (set-face-attribute 'variable-pitch nil :family "Anonymice Nerd Font Mono" :height 1.0)

 ;; Monospaced typeface
 (set-face-attribute 'fixed-pitch nil :family "Spleen 32x64" :height 1.5)


 (if (facep 'mode-line-active)
     (set-face-attribute 'mode-line-active nil :family "Spleen 32x64" :height 200) ; For 29+
   (set-face-attribute 'mode-line nil :family "Spleen 32x64" :height 200))
(set-face-attribute 'mode-line-inactive nil :family "Anonymice Nerd Font Mono" :height 200)

(use-package helpful
:straight (helpful :repo "Wilfred/helpful" :fetcher github)

  :ensure t)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)

(global-set-key (kbd "C-h C") #'helpful-command)

(use-package popper
:straight (popper :fetcher github :repo "karthink/popper")
  :ensure t
  :bind (("C-`" . popper-toggle-latest) 
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  )


(defvar rcool/help-temp-buffers '("^\\*Help\\*$",
                                  "^\\*helpful command"))

(while rcool/help-temp-buffers
  (add-to-list 'display-buffer-alist
               `(,(car rcool/help-temp-buffers)
                 (display-buffer-in-side-window
                  (side . left)
                  (window-width . 50))))

  (setq rcool/help-temp-buffers (cdr rcool/help-temp-buffers)))

(use-package general
   :ensure t
   :straight (general
:fetcher github
:repo "noctuid/general.el")
   :config
   (general-auto-unbind-keys)
   (general-evil-setup t))

(use-package ace-window 
:straight (ace-window :repo "abo-abo/ace-window"
        :fetcher github)
:defer t :ensure t)

(use-package diminish 
:straight (diminish :fetcher github :repo "myrjola/diminish.el")
    :ensure t)

(use-package bufler
  :ensure t
  :straight (bufler :fetcher github :repo "alphapapa/bufler.el"
    :files (:defaults (:exclude "helm-bufler.el")))
  :config
  (setq bufler-filter-buffer-modes nil
       bufler-filter-buffer-name-regexps nil)
  (setf bufler-groups (bufler-defgroups
                       (group
                        ;; All named workspaces
                        (auto-workspace))
                       (group
                        ;; `help-mode' and `info-mode'
                        (group-or "*Help/Info*"
                                  (mode-match "*Help*" (rx bos "help-"))
                                  (mode-match "*Info*" (rx bos "info-"))))
                       (group
                        ;;special buffers (except magit)
                        (group-and "*Special*"
                                   (lambda (buffer)
                                     (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                                          buffer)
                                                 (funcall (mode-match "Dired" (rx bos "dired"))
                                                          buffer)
                                                 (funcall (auto-file) buffer))
                                       "*Special*"))))
                       (group
                        ;;Special Special Buffers
                        (name-match "**Special**"
                                    (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
                       (group
                        ;; All other Magic buffers by directory
                        (mode-match "Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
                        (auto-directory))
                       ;; Remaining by directory then major mode
                       (auto-directory)
                       (auto-mode)))
  :general
  (:keymaps 'bufler-list-mode-map "Q" 'kill-this-buffer))

(use-package avy :ensure t :straight (avy :repo "abo-abo/avy"
     :fetcher github))

(use-package undo-fu :ensure t :straight (undo-fu :fetcher codeberg :repo "ideasman42/emacs-undo-fu"))

(use-package flycheck :straight (flycheck :repo "flycheck/flycheck" :fetcher github)
:ensure t
  :config
(global-flycheck-mode))

(use-package exec-path-from-shell :straight (exec-path-from-shell :fetcher github :repo "purcell/exec-path-from-shell")
:ensure t
:config
(exec-path-from-shell-initialize))

(defun modeline-face-color ()
    (let ((color (cond ((minibufferp) '("#1d1d1f" . "#ffffff"))
		       ((evil-insert-state-p) '("#331612" . "#ffffff"))
		       ((evil-emacs-state-p) '("#1a2b14" . "#ffffff"))
		       ((evil-visual-state-p) '("#5a5c21" . "#ffffff"))
		       ((evil-replace-state-p) '("#1a403e" . "#ffffff"))
		       ((evil-motion-state-p) '("#677691" . "#ffffff"))
		       ((buffer-modified-p) '("#2b1a40" . "#ffffff"))
		       (t '("#1d1d1f" . "#ffffff")))))
      (set-face-background 'mode-line (car color))
      (set-face-foreground 'mode-line (cdr color))))
  (use-package evil
    :ensure t
    :straight (evil :repo "emacs-evil/evil"
      :fetcher github
      :files (:defaults
	      "doc/build/texinfo/evil.texi"
	      (:exclude "evil-test-helpers.el")))
    :init
    (setq evil-want-fine-under t)
    (setq evil-want-keybinding nil)
    (setq evil-want-Y-yank-to-eol t)
    (setq evil-undo-system 'undo-fu)
    (setq evil-search-module 'evil-search)
    :config

    (evil-set-initial-state 'dashboard-mode 'motion)
    (evil-set-initial-state 'debugger-mode 'motion)
    (evil-set-initial-state 'pdf-view-mode 'motion)
    (evil-set-initial-state 'bufler-list-mode 'emacs)
    (evil-set-initial-state 'inferior-python-mode 'emacs)
    (evil-set-initial-state 'term-mode 'emacs)
    (evil-set-initial-state 'calc-mode 'emacs)
    (define-key evil-window-map "\C-q" 'evil-delete-buffer) ;; Maps C-w C-q to evil-delete-buffer (The first C-w puts you into evil-window-map)
    (define-key evil-window-map "\C-w" 'kill-this-buffer)
    (define-key evil-motion-state-map "\C-b" 'evil-scroll-up) ;; Makes C-b how C-u is
    (add-hook 'post-command-hook 'modeline-face-color)

    ;; ----- Setting cursor colors
    (setq evil-emacs-state-cursor    '("#649bce" box))
    (setq evil-normal-state-cursor   '("#d9a871" box))
    (setq evil-operator-state-cursor '("#ebcb8b" hollow))
    (setq evil-visual-state-cursor   '("#677691" box))
    (setq evil-insert-state-cursor   '("#eb998b" (bar . 2)))
    (setq evil-replace-state-cursor  '("#eb998b" hbar))
    (setq evil-motion-state-cursor   '("#ad8beb" box))
(with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
		   (kbd "<tab>") #'my/copilot-tab))
    (evil-mode +1))
  (use-package evil-collection
    :ensure t
    :after evil
    :straight
    (evil-collection :fetcher github
		 :repo "emacs-evil/evil-collection"
		 :files (:defaults "modes"))
  :diminish
    :config
    (setq evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) calc diff-mode))
    (evil-collection-init))

(use-package evil-surround
  :ensure t 
  :straight 
  (evil-surround :repo "emacs-evil/evil-surround" :fetcher github :old-names (surround))
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-exchange
  :straight (evil-exchange :fetcher github :repo "Dewdrops/evil-exchange")
:ensure t
  :config
(evil-exchange-install)
)

(use-package evil-visualstar
  :straight (evil-visualstar :repo "bling/evil-visualstar" :fetcher github)
:ensure t
  :config
(global-evil-visualstar-mode 1))

(use-package evil-dvorak :ensure t :straight (evil-dvorak :fetcher github  :repo "jbranso/evil-dvorak"))

(use-package evil-escape
 :ensure t

:straight (evil-escape :fetcher github :repo "emacsorphanage/evil-escape")
 :custom
 (setq-default evil-escape-key-sequence "jk")
 (setq-default evil-escape-key-delay 0.5))

(use-package evil-god-state
    :ensure t
  :straight (evil-god-state :host github
:repo "gridaphobe/evil-god-state"
:files (:defaults))
    :after evil
    :diminish evil-god-state
    :config
    (define-key evil-normal-state-map (kbd "'") 'evil-execute-in-god-state)
    (define-key evil-god-state-map (kbd "Esc") 'evil-god-state-bail)
    )

(use-package copilot
      :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
      :ensure t
:config

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
)

(use-package yasnippet
 :ensure t
:straight (yasnippet :repo "joaotavora/yasnippet"
         :fetcher github
         :files ("yasnippet.el" "snippets")) 
 :config
 (setq yas-snippet-dirs '("~/.doom.d/snippets"))
 (yas-global-mode 1))

(use-package which-key
  :ensure t
  :straight (which-key :repo "justbur/emacs-which-key"
        :fetcher github)
  :init (which-key-mode)
  :diminish which-key-mode
  :config
(setq which-key-idle-delay 0.01))

(use-package rainbow-delimiters
 :ensure t
 :straight (rainbow-delimiters :fetcher github :repo "Fanael/rainbow-delimiters")
:hook (prog-mode . rainbow-delimiters-mode))

(use-package hydra :ensure t :straight t)
(defhydra hydra-text-scale (:timeout 4)
          "scale text"
          ("j" text-scale-increase "in")
          ("k" text-scale-decrease "out")
          ("f" nil "finished" :exit t))

(use-package quickrun
   :defer t
   :ensure t
   :straight (quickrun
:repo "emacsorphanage/quickrun"
:fetcher github)
   :general
   (general-def
    :states 'normal
    :prefix "SPC"
    :keymaps 'quickrun--mode-map
    "cq" '(nil :which-key "quickrun")
    "cqq" '(quit-window :which-key "Quit")
    "cqr" '(quickrun :which-key "Run")
    "cqR" '(quickrun-region :which-key "Run Region")
    "cqa" '(quickrun-with-arg :which-key "Run with [A]rgs")
    "cqm" '(quickrun-autorun-mode :which-key "Toggle autorun mode")
    "cqs" '(quickrun-select :which-key "Select backend")"cq" '(nil :which-key "quickrun")
    "cqq" '(quit-window :which-key "Quit")
    "cqr" '(quickrun :which-key "Run")
    "cqR" '(quickrun-region :which-key "Run Region")
    "cqa" '(quickrun-with-arg :which-key "Run with [A]rgs")
    "cqm" '(quickrun-autorun-mode :which-key "Toggle autorun mode")
    "cqs" '(quickrun-select :which-key "Select backend")
))

(use-package rainbow-mode :ensure t :diminish :straight (rainbow-mode :repo "emacsmirror/rainbow-mode" :fetcher github)
)
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
		     (list 'org-agenda-mode)))
      (rainbow-mode 1))))
(global-rainbow-mode 1)

(use-package recentf

             :ensure nil
             :config
             (setq recentf-max-saved-items 200)
             (setq recentf-filename-handlers
                   (append '(abbreviate-file-name) recentf-filename-handlers))
(recentf-mode))

(use-package projectile :straight (projectile :fetcher github :repo "bbatsov/projectile")
  :ensure t
:diminish
  )

(use-package perspective
:straight (perspective :fetcher github :repo "nex3/perspective-el")
:ensure t
    :bind
    ("C-x C-b" . persp-list-buffers)
    :custom
    (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (setq persp-initial-frame-name "Main")
  (unless (equal persp-mode t)
    (persp-mode)))

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(setq tramp-default-method "ssh")

(use-package org
:straight nil
  :ensure t
  :hook (org-mode . rc/prettify-symbols-setup)
  :hook (org-capture-mode . evil-insert-state)
  :diminish org-indent-mode
  :diminish visual-line-mode
  :config

  ;; Visuals
  (setq org-src-fontify-natively t)
  (setq org-highlight-latex-and-related '(native))
  (setq org-startup-folded 'showeverything)
  (setq org-image-actual-width 300)
  (setq org-fonitfy-whole-heading-line t)

  ;; Interaction
  (setq org-cycle-separator-lines 1 )
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-scr-tab-acts-natively t)

  (setq org-M-RET-may-splitline '((headline) (item . t) (table .t) (default)))
  (setq org-loop-over-headlines-in-active-region nil)

  ;;Opens links to other org file in same frame
  (setq org-link-frame-setup '((file . find-file)))

  (setq org-log-done t
        org-log-into-drawer t)

  ;; Automatically change bullet type when indenting
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-scr-tab-acts-natively t)
  (setq org-M-RET-may-splitline '((headline) (item . t) (table .t) (default)))
  (setq org-loop-over-headlines-in-active-region nil)

  ;;Opens links to other org file in same frame
  (setq org-link-frame-setup '((file . find-file)))

  (setq org-log-done t
        org-log-into-drawer t)

  ;; Automatically change bullet type when indenting
  (setq org-list-demote-modify-bullet
        '(("+" . "*")("*" . "-")("-" . "+")))

  ;; Automatically save and close the org files i archive to
  (dolist (file '("todo-archive.org_archive"))
    (advice-add 'org-advice-subtree-default :after
                (lambda () (rc/save-and-close-this-buffer file))))

  (setq org-list-demote-modify-bullet
        '(("+" . "*")("*" . "-")("-" . "+")))

  ;; Automatically save and close the org files i archive to
  (dolist (file '("todo-archive.org_archive"))
    (advice-add 'org-advice-subtree-default :after
                (lambda () (rc/save-and-close-this-buffer file))))

(setq org-tags-column -1)

(setq org-todo-keywords '((type
                           "TODO(t)" "WAITING(h)" "INPROG-TODO(i)" "WORK(w)"
                           "STUDY(s)" "SOMEDAY" "READ(r)" "PROJ(p)" "CONTACT(c)"
                           "|" "DONE(d)" "CANCELLED(C)")))

(setq org-todo-keyword-faces
      '(("TODO"  :inherit (region org-todo) :foreground "DarkOrange1"   :weight bold)
        ("WORK"  :inherit (org-todo region) :foreground "DarkOrange1"   :weight bold)
        ("READ"  :inherit (org-todo region) :foreground "MediumPurple3" :weight bold)
        ("PROJ"  :inherit (org-todo region) :foreground "orange3"     :weight bold)
        ("STUDY" :inherit (region org-todo) :foreground "plum3"       :weight bold)
        ("DONE" . "SeaGreen4")))

(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 . "red2")
        (66 . "Gold1")
        (67 . "Goldenrod2")
        (68 . "PaleTurquoise3")
        (69 . "DarkSlateGray4")
        (70 . "PaleTurquoise4")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   (js . t)
   (lua . t)
   (sql . t)
   ))
(setq python-shell-completion-native-enable nil)
(setq org-src-window-setup 'current-window)

 (defun org-babel-execute:typescript (body params)
  (let ((org-babel-js-cmd "npx ts-node < "))
    (org-babel-execute:js body params)))

; custom time stamp format. I don't use this.
(setq org-time-stamp-custom-formats '("<%A, %B %d, %Y" . "<%m/%d/%y %a %I:%M %p>"))

(setq org-agenda-restore-windows-after-quit t)

(setq org-agenda-window-setup 'current-window)

;; Only show upcoming deadlines for the next 5 days. By default it shows
;; 14 days into the future, which seems excessive.
(setq org-deadline-warning-days 5)
;; If something is done, don't show its deadline
(setq org-agenda-skip-deadline-if-done t)
;; If something is done, don't show when it's scheduled for
(setq org-agenda-skip-scheduled-if-done t)
;; If something is scheduled, don't tell me it is due soon
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; use AM-PM and not 24-hour time
(setq org-agenda-timegrid-use-ampm 1)

;; A new day is 3am (I work late into the night)
(setq org-extend-today-until 3)

;; (setq org-agenda-time-grid '((daily today require-timed)
;;                              (1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
;;                              "        " "----------------"))

(setq org-agenda-time-grid nil)

(setq org-agenda-span 'day)

;; (setq org-agenda-block-separator ?-)
(setq org-agenda-current-time-string "<----------------- Now")

(setq org-agenda-scheduled-leaders '("Plan | " "Sched.%2dx: ") ; ⇛
      org-agenda-deadline-leaders '("Due: " "Due in %1d d. | " "Due %1d d. ago: "))

(setq org-agenda-prefix-format '((agenda . "  %-6:T %t%s")
                                 (todo . "  ")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (setq-local line-spacing 4)))

(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t)          ; Show full paths for refiling

;; By default an org-capture/refile will save a bookmark. This
;; disables that and keeps my bookmark list how I want it.


(setq org-capture-templates
'(
        ("w" "Work Todo Entries")
        ("we" "No Time" entry (file "~/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title} %?" :prepend t :empty-lines-before 0
         :refile-targets (("~/org/work.org" :maxlevel . 2)))

        ("ws" "Scheduled" entry (file "~/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
         :refile-targets (("~/org/work.org" :maxlevel . 2)))

        ("wd" "Deadline" entry (file "~/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
         :refile-targets (("~/org/work.org" :maxlevel . 2)))

        ("ww" "Scheduled & deadline" entry (file "~/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0
         :refile-targets (("~/org/work.org" :maxlevel . 2)))

)
))

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "t" 'org-todo
  "<return>" 'org-open-at-point-global
  "K" 'org-shiftup
  "J" 'org-shiftdown
  "`" 'org-ctrl-c-ctrl-c)
(general-def
  :states 'insert
  :keymaps 'org-mode-map
  "C-o" 'evil-org-open-above)

(general-def
  :keymaps 'org-mode-map
  "M-[" 'org-metaleft
  "M-]" 'org-metaright
  "s-r" 'org-refile
  "M-k" 'org-insert-link
  )

;; Org src for editing a source block
(general-def
  :prefix ","
  :states 'normal
  :keymaps 'org-src-mode-map

  "b" '(nil :which-key "org src")
  "bb" 'org-edit-src-exit
  "bc" 'org-edit-src-abort)

(general-def
 :prefix ","
 :states 'motion
 :keymaps '(org-mode-map)
 "" nil

 "A" '(org-archive-subtree-default :which-key "org-archive")
 "a" '(org-agenda :which-key "org agenda")
 "6" '(org-sort :which-key "sort")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-schedule :which-key "schedule")
 "S" '(jib/org-schedule-tomorrow :which-key "schedule tmrw")
 "d" '(org-deadline :which-key "deadline")
 "g" '(consult-org-heading :which-key "goto heading")
 "t" '(org-tag :which-key "set tags")
 "p" '(org-set-property :which-key "set property")
 "r" '(jib/org-refile-this-file :which-key "refile in file")
 "e" '(org-export-dispatch :which-key "export org")
 "B" '(org-toggle-narrow-to-subtree :which-key "toggle narrow to subtree")
 "," '(jib/org-set-startup-visibility :which-key "startup visibility")
 "H" '(org-html-convert-region-to-html :which-key "convert region to html")
 "C" '(jib/org-copy-link-to-clipboard :which-key "copy link to clipboard")
 "1" '(org-toggle-link-display :which-key "toggle link display")
 "2" '(org-toggle-inline-images :which-key "toggle images")
 "3" '(jib/org-occur-unchecked-boxes :which-key "occur unchecked boxes")

 "b" '(nil :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle")
 "bb" '(org-edit-special :which-key "org-edit-special")
 "bc" '(org-edit-src-abort :which-key "org-edit-src-abort")
 "bk" '(org-babel-remove-result-one-or-many :which-key "org-babel-remove-result-one-or-many")


 ;; insert
 "i" '(nil :which-key "insert")


 "il" '(org-insert-link :which-key "org-insert-link")
 "l" '(org-insert-link :which-key "org-insert-link") ;; More convenient access
 "iL" '(org-link :which-key "org-link")
 "it" '(jb-hydra-org-table/body :which-key "tables")

 "is" '(nil :which-key "insert stamp")
 "iss" '((lambda () (interactive) (call-interactively (org-time-stamp-inactive))) :which-key "org-time-stamp-inactive")
 "isS" '((lambda () (interactive) (call-interactively (org-time-stamp nil))) :which-key "org-time-stamp")

 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-clock-in :which-key "clock in")
 "co" '(org-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )


;; Org-agenda
(general-def
 :prefix ","
 :states 'motion
 :keymaps '(org-agenda-mode-map) ;; Available in org mode, org agenda
 "" nil
 "a" '(org-agenda :which-key "org agenda")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-agenda-schedule :which-key "schedule")
 "d" '(org-agenda-deadline :which-key "deadline")
 "t" '(org-agenda-set-tags :which-key "set tags")
 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-agenda-clock-in :which-key "clock in")
 "co" '(org-agenda-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )

(evil-define-key 'motion org-agenda-mode-map
  (kbd "f") 'org-agenda-later
  (kbd "b") 'org-agenda-earlier)

(fset 'evil-redirect-digit-argument 'ignore) ;; before evil-org loaded
  (use-package evil-org
    :ensure t
    :straight (evil-org :fetcher github :repo "Somelauw/evil-org-mode")
    :diminish evil-org-mode
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook'
              (lambda () (evil-org-set-key-theme))))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

(use-package org-auto-tangle
  :ensure t
  :straight (org-auto-tangle :repo "yilkalargaw/org-auto-tangle" :fetcher github)
  :defer t
:diminish
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(use-package org-projectile
:straight (org-projectile :host github :repo "IvanMalison/org-projectile")
  :after org org-category-capture
       :config
       (progn
	 (setq org-projectile-projects-file
	       "~/org/projects.org"
	       org-projectile-capture-template
	       ("* TODO %?\n %i\n %a")
	       )
	 (add-to-list 'org-capture-templates
		      (org-projectile-project-todo-entry
		       :capture-character "l"
		       :capture-heading "Linked Project TODO"))
	 (add-to-list 'org-capture-templates
		      (org-projectile-project-todo-entry
		       :capture-character "p"))

	 (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
	 (push (org-projectile-project-todo-entry) org-capture-templates))
       :ensure t

       )

(use-package tree-sitter
  :ensure t
  :straight (tree-sitter :repo "emacs-tree-sitter/elisp-tree-sitter"
           :fetcher github
           :branch "release"
           :files (:defaults (:exclude "lisp/tree-sitter-tests.el")))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(add-to-list 'tree-sitter-major-mode-language-alist '(tex-mode . tsx))
(use-package tree-sitter-langs
:straight (tree-sitter-langs :repo "emacs-tree-sitter/tree-sitter-langs"
                 :fetcher github
                 :branch "release"
                 :files (:defaults
                         "queries"))
  :ensure t
  :after tree-sitter)

(use-package lsp-mode
  :ensure t
:straight t 
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode . #'lsp)
	 (tsx-ts-mode . #'lsp)
	 (js-mode . #'lsp)
	 (lua-mode . #'lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-modeline-code-actions-segments t)
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))


    (use-package lsp-ui
:straight t
    :ensure t
    :hook ((lsp-mode . lsp-ui-mode)
(lsp-mode . lsp-ui-sideline-mode))
    :config
(setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-position 'top))

(straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
(require 'tsi-css)
(require 'tsi-json)
(require 'tsi-typescript)
(add-hook 'tsx-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
(add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1)))

(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs29"))
(add-to-list 'auto-mode-alist '("\\.[jt]sx\?\\'" . tsx-mode))
(general-def
  :prefix ","
  :states 'motion
  :keymaps 'general-override-mode-map
  "" nil

  "z" '(tsx-mode-fold-toggle-all-nodes :which-key "Toggle Fold all nodes")
  "c" '(tsx-mode-coverage-toggle :which-key "Toggle Coverage")
  "tab" '(tsx-mode-fold-toggle-node :which-key "Toggle Fold here")
 )

(use-package apheleia
  :ensure t
:diminish
:straight '(apheleia :type git :host github :repo "raxod502/apheleia")
  :config
  (apheleia-global-mode +1))

(use-package lua-mode
  :straight (lua-mode :repo "immerrr/lua-mode" :fetcher github)
    :ensure t
    :after (tree-sitter lsp)

:init
    (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  )

;; Example configuration for Consult
(use-package consult
:straight (consult :type git :host github :repo "minad/consult")
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
   (autoload 'projectile-project-root "projectile")
   (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git"))))
   )

(use-package consult-dir
  :ensure t
:straight (consult-dir :host github :repo "karthink/consult-dir")
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-flycheck
        :ensure t
      :straight (
consult-flycheck
  :host github
:repo
"minad/consult-flycheck"
  ))

(use-package consult-lsp :ensure t :straight t)

(use-package consult-projectile :ensure t
      :straight
     (consult-projectile
    :type git :host gitlab
  :repo "OlMon/consult-projectile"
:branch "master"))

(use-package consult-yasnippet
      :ensure t
:straight (consult-yasnippet :host github
  :repo "mohkale/consult-yasnippet")
  )

(use-package wgrep :ensure t :straight (wgrep
 :fetcher github
 :repo "mhayashi1120/Emacs-wgrep"
 :files ("wgrep.el")))

;; Enable rich annotations using the Marginalia package
  (use-package marginalia 
    :straight (marginalia :host github :repo "minad/marginalia")
    ;; Either bind `marginalia-cycle' globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

    ;; The :init configuration is always executed (Not lazy!)
    :init
:general
(general-def
  :states 'normal
:keymap 'override
"?" 'marginalia-cyle)
    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode))

;; Enable vertico
  (use-package vertico
    :ensure t
:straight (vertico :host github :repo "minad/vertico"  :files (:defaults "extensions/*")
                       :includes (vertico-buffer
                                  vertico-directory
                                  vertico-flat
                                  vertico-indexed
                                  vertico-mouse
                                  vertico-quick
                                  vertico-repeat
                                  vertico-reverse)))
    :init
    (vertico-mode)

    ;; Different scroll margin
    ;; (setq vertico-scroll-margin 0)

    ;; Show more candidates
    ;; (setq vertico-count 20)

    ;; Grow and shrink the Vertico minibuffer
    ;; (setq vertico-resize t)

    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    ;; (setq vertico-cycle t)


  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
    :ensure t
    :init
    (savehist-mode))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
		    (replace-regexp-in-string
		     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		     crm-separator)
		    (car args))
	    (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t))

;; Configure directory extension.
(use-package vertico-directory  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless :ensure t
:straight (orderless :host github :repo "oantolin/orderless") 
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico-quick :ensure nil)

(use-package embark
:straight (embark :host github :repo "oantolin/embark")
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
:straight (embark-consult :host github :repo "oantolin/embark")
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package treemacs
  :straight (treemacs :host github :repo "Alexander-Miller/treemacs" :branch "master")
    :ensure t
    :defer t
    :config
    (setq treemacs-position 'right
	  width 50))

  (use-package lsp-treemacs
  :ensure t
  :straight t
:after lsp)

(use-package treemacs-evil
:straight (treemacs-evil :host github :repo "Alexander-Miller/treemacs" :branch "master")
  :ensure t
  :config
  )

(use-package treemacs-projectile
:straight (treemacs-projectile :host github :repo "Alexander-Miller/treemacs" :branch "master")
  :ensure t
  )

(use-package treemacs-magit
:straight (treemacs-magit :host github :repo "Alexander-Miller/treemacs" :branch "master")

  :ensure t
  )

(use-package treemacs-icons-dired :ensure t 
:straight (treemacs-icons-dired :host github :repo "Alexander-Miller/treemacs" :branch "master")
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-perspective :ensure t
:straight (treemacs-perspective :host github :repo "Alexander-Miller/treemacs" :branch "master")
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-all-the-icons :ensure t 
:straight (treemacs-all-the-icons :host github :repo "Alexander-Miller/treemacs" :branch "master")
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package prescient
        :straight t
          :ensure t)
      (use-package vertico-prescient :ensure t :straight t
      :config
(vertico-prescient-mode +1)
  )

(use-package magit
:straight t
:ensure t
:commands (magit-status magit-get-current-branch)
:custom
(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
:straight (evil-nerd-commenter :host github :repo "redguardtoo/evil-nerd-commenter")
  :ensure t
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

(defhydra rc-hydra-variable-fonts (:pre (mixed-pitch-mode 0)
                                        :post (mixed-pitch-mode 1))
  ("t" (set-face-attribute 'variable-pitch nil :family "Tinos" :height 160)             "Tinos")
  ("g" (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160 :weight 'normal)             "EB Garamond")
  ("n" (set-face-attribute 'variable-pitch nil :family "Nunito" :slant 'normal :weight 'normal :height 160) "Nunito")
  ("s" (set-face-attribute 'variable-pitch nil :family "Spleen 32x64" :slant 'normal :weight 'normal :height 160) "Spleen")
  ("c" (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :slant 'normal :weight 'normal :height 160) "Source Sans Pro")
)

(defhydra hydra-dired (:hint nil :color pink)
      "
    _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
    _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
    _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
    _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
    _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
    _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
    _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
    _z_ compress-file  _A_ find regexp
    _Z_ compress       _Q_ repl regexp

    T - tag prefix
    "
      ("\\" dired-do-ispell)
      ("(" dired-hide-details-mode)
      (")" dired-omit-mode)
      ("+" dired-create-directory)
      ("=" diredp-ediff)         ;; smart diff
      ("?" dired-summary)
      ("$" diredp-hide-subdir-nomove)
      ("A" dired-do-find-regexp)
      ("C" dired-do-copy)        ;; Copy all marked files
("D" dired-do-delete)
      ("E" dired-mark-extension)
      ("e" dired-ediff-files)
      ("F" dired-do-find-marked-files)
      ("G" dired-do-chgrp)
      ("g" revert-buffer)        ;; read all directories again (refresh)
      ("i" dired-maybe-insert-subdir)
      ("l" dired-do-redisplay)   ;; relist the marked or singel directory
      ("M" dired-do-chmod)
      ("m" dired-mark)
      ("O" dired-display-file)
      ("o" dired-find-file-other-window)
      ("Q" dired-do-find-regexp-and-replace)
      ("R" dired-do-rename)
      ("r" dired-do-rsynch)
      ("S" dired-do-symlink)
      ("s" dired-sort-toggle-or-edit)
      ("t" dired-toggle-marks)
      ("U" dired-unmark-all-marks)
      ("u" dired-unmark)
      ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
      ("w" dired-kill-subdir)
      ("Y" dired-do-relsymlink)
      ("z" diredp-compress-this-file)
      ("Z" dired-do-compress)
      ("q" nil)
      ("." nil :color blue))

    (define-key dired-mode-map "." 'hydra-dired/body)

(defhydra rc-hydra-theme-switcher (:hint nil)
           "
            Dark                ^Light^
       ----------------------------------------------
       _1_ one              _z_ one-light 
       _2_ vivendi          _x_ operandi
       _3_ molokai          _c_ ef-trio-light
       _4_ snazzy          MORE DARK BELOW
       _5_ old-hope         _v_     ef-dark
       _6_ henna            _b_     doom-dark+  
       _7_ kaolin-galaxy    _n_     iosevkm     
       _8_ monokai-machine  _m_     vibrant              
       _9_ xcode                ^
       _0_ moonlight            ^
       _-_ laserwave            ^    
       _q_ quit                 ^
       ^                        ^
         "
           ;; Dark
           ("1" (consult-theme 'doom-one )             "one")
           ("2" (consult-theme 'modus-vivendi )             "modus-vivendi")
           ("3" (consult-theme 'doom-molokai )             "molokai")
           ("4" (consult-theme 'doom-snazzy t )             "snazzy")
           ("5" (consult-theme 'doom-old-hope )             "old hope")
           ("6" (consult-theme 'doom-henna )             "henna")
           ("7" (consult-theme 'kaolin-galaxy )             "jaolin-galaxy")
           ("8" (consult-theme 'doom-monokai-machine )             "monokai-machine")
           ("9" (consult-theme 'doom-xcode )             "xcode")
           ("0" (consult-theme 'doom-moonlight )             "moonlight")
           ("-" (consult-theme 'doom-laserwave )             "laserwave")
           ("z" (consult-theme 'doom-one-light )
            "one-light")
           ("x" (consult-theme 'modus-operandi )             "operand")
("c" (consult-theme 'ef-trio-light )             "ef-trio-light")
("v" (consult-theme 'ef-dark )             "ef-dark")
("b" (consult-theme 'doom-dark+ )             "doom-dark+")
("n" (consult-theme 'doom-Iosvkem )             "Iosvkem")
("m" (consult-theme 'doom-vibrant )             "vibrant")
 ("q" nil)

           )

(defhydra rc-hydra-window (:hint nil)
   "
Movement      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------------
_M-<left>_  <   _s_ vertical      _b_uffer        _<left>_  <
_M-<right>_ >   _v_ horizontal    _f_ind file     _<down>_  ↓
_M-<up>_    ↑   _m_aximize        _x_wap          _<up>_    ↑
_M-<down>_  ↓   _c_lose           _[_backward     _<right>_ >
_q_uit          _e_qualize        _]_forward     ^
^               ^               _K_ill         ^
^               ^                  ^             ^
"
   ;; Movement
   ("M-<left>" windmove-left)
   ("M-<down>" windmove-down)
   ("M-<up>" windmove-up)
   ("M-<right>" windmove-right)

   ;; Split/manage
   ("s" rc/split-window-vertically-and-switch)
   ("v" rc/split-window-horizontally-and-switch)
   ("c" evil-window-delete)
   ("d" evil-window-delete)
   ("m" delete-other-windows)
   ("e" balance-windows)

   ;; Switch
   ("b" consult-switch-buffer)
   ("f" consult-find)
   ("P" project-find-file)
   ("x" ace-swap-window)
   ("[" previous-buffer)
   ("]" next-buffer)
   ("K" kill-this-buffer)

   ;; Resize
   ("<left>" windresize-left)
   ("<right>" windresize-right)
   ("<down>" windresize-down)
   ("<up>" windresize-up)

   ("q" nil))

(defhydra hydra-flycheck
   (:pre (flycheck-list-errors)
    :post (quit-windows-on "*Flycheck errors*")
    :hint nil)
 "Errors"
 ("f" flycheck-error-list-set-filter "Filter")

 ("j" flycheck-next-error "Next")
 ("k" flycheck-previous-error "Previous")
 ("gg" flycheck-first-error "First")
 ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
 ("q" nil))

;; Hydra for org agenda (graciously taken from Spacemacs)
      (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                       :post (setq which-key-inhibit nil)
                                       :hint none)
        "
      Org agenda (_q_uit)

      ^Clock^      ^Visit entry^              ^Date^             ^Other^
      ^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
      _ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
      _co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
      _cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
      _cj_ jump    _o_   link                 _+_  do later      ^^
      ^^           ^^                         _-_  do earlier    ^^
      ^^           ^^                         ^^                 ^^
      ^View^          ^Filter^                 ^Headline^         ^Toggle mode^
      ^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
      _vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
  :keymaps 'override
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
      _vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
      _vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
      _vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
      _vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
      _vp_ prev span  ^^                       ^^                 ^^
      _vr_ reset      ^^                       ^^                 ^^
      ^^              ^^                       ^^                 ^^
      "
        ;; Entry
        ("hA" org-agenda-archive-default)
        ("hk" org-agenda-kill)
        ("hp" org-agenda-priority)
        ("hr" org-agenda-refile)
        ("h:" org-agenda-set-tags)
        ("ht" org-agenda-todo)
        ;; Visit entry
        ("o"   link-hint-open-link :exit t)
        ("<tab>" org-agenda-goto :exit t)
        ("TAB" org-agenda-goto :exit t)
        ("SPC" org-agenda-show-and-scroll-up)
        ("RET" org-agenda-switch-to :exit t)
        ;; Date
        ("dt" org-agenda-date-prompt)
        ("dd" org-agenda-deadline)
        ("+" org-agenda-do-date-later)
        ("-" org-agenda-do-date-earlier)
        ("ds" org-agenda-schedule)
        ;; View
        ("vd" org-agenda-day-view)
        ("vw" org-agenda-week-view)
        ("vt" org-agenda-fortnight-view)
        ("vm" org-agenda-month-view)
        ("vy" org-agenda-year-view)
        ("vn" org-agenda-later)
        ("vp" org-agenda-earlier)
        ("vr" org-agenda-reset-view)
        ;; Toggle mode
        ("ta" org-agenda-archives-mode)
        ("tA" (org-agenda-archives-mode 'files))
        ("tr" org-agenda-clockreport-mode)
        ("tf" org-agenda-follow-mode)
        ("tl" org-agenda-log-mode)
        ("td" org-agenda-toggle-diary)
        ;; Filter
        ("fc" org-agenda-filter-by-category)
        ("fx" org-agenda-filter-by-regexp)
        ("ft" org-agenda-filter-by-tag)
        ("fr" org-agenda-filter-by-tag-refine)
        ("fh" org-agenda-filter-by-top-headline)
        ("fd" org-agenda-filter-remove-all)
        ;; Clock
        ("cq" org-agenda-clock-cancel)
        ("cj" org-agenda-clock-goto :exit t)
        ("ci" org-agenda-clock-in :exit t)
        ("co" org-agenda-clock-out)
        ;; Other
        ("q" nil :exit t)
        ("gd" org-agenda-goto-date)
        ("." org-agenda-goto-today)
        ("gr" org-agenda-redo))

(defhydra hydra-avy (:exit t :hint nil)
      "
     Line^^       Region^^        Goto
    ----------------------------------------------------------
     [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
     [_m_] move   [_M_] move      [_w_] word        [_W_] any word
     [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
      ("c" avy-goto-char-timer)
      ("C" avy-goto-char)
      ("w" avy-goto-word-1)
      ("W" avy-goto-word-0)
      ("l" avy-goto-line)
      ("L" avy-goto-end-of-line)
      ("m" avy-move-line)
      ("M" avy-move-region)
("k" avy-kill-whole-line)
      ("K" avy-kill-region)
      ("y" avy-copy-line)
      ("Y" avy-copy-region))

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

(general-def
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   ;; Top level functions
   "/" '(rc/rg :which-key "RipGrep")
   "SPC" '(M-x :which-key "M-x")
   "q" '(popper-kill-latest-popup :which-key "kill popup")
   "X" '(org-capture :which-key "Capture")

"f" '(nil :which-key "Files")
"fd" '(ido-dired :which-key "Find directory")
"ff" '(ido-find-file :which-key "Find file")
"fr" '(consult-recent-files :which-key "Recent Files")
"fs" '(save :which-key "Save")

;; Buffers
"b" '(nil :which-key "Buffer")
"bb" '(consult-project-buffer :which-key "Switch (proj) buffer")
"bB" '(buffler-switch-buffer :which-key "Switch any buffer")
"bd" '(evil-delete-buffer :which-key "Delete buffer")
"bm" '(rc/kill-other-buffers :which-key "Kill other buffers")
"bi" '(ibuffer :which-key "iBuffer")
"br" '(revert-buffer :which-key "Revert buffer")
"bn" '(next-buffer :which-key "Next")
"bp" '(previous-buffer :which-key "Prev")

;; lCode

"c" '(nil :which-key "Code")
"cd" '(xref-find-definition :which-key "Definition")
"cR" '(xref-find-references :which-key "References")
"ca" '(lsp-execute-code-actions :which-key "Code action")
"ci" '(lsp-ui-peek-find-implementation :which-key "Implementations")
"cD" '(lsp-ui-peek-findre-type-definition :which-key "Type Def.")
"cr" '(lsp-rename :which-key "Rename Symbol")



"ce" '(nil :which-key "Errors")
"cek" '(flycheck-previous-error :which-key "Prev Error")
"cej" '(flycheck-next-error :which-key "Next Error")
"ceg" '(flycheck-first-error :which-key "First Error")
"cef" '(flycheck-error-list-set-filter :which-key "Filter")
"ceG" '((progn (goto-char (point-max)) (flycheck-previour-error)) :which-key "Last Error")
"cel" '(consult-flycheck :which-key "Error List")
"cee" '(flycheck-explain-error-at-point :which-key "Explain error")

"cf" '(nil :which-key "Format")
"cfl" '(lsp-format-buffer :which-key "LSP format Buffer")
"cfa" '(apheleia-format-buffer :which-key "Apheleia Format")
"cfr" '(lsp-format-region :which-key "LSP Format Region")

"m" '(nil :which-key "Bookmarks")
"ma" '(bookmark-set  :which-key "Set a bookmark")
"mj" '(bookmark-jump-other-window :which-key "Jump to bookmark")
"mJ" '(bookmark-jump :which-key "Jump Here")
"mb" '(consult-bookmark :which-key "Set or Jump")

"p" '(nil :which-key "Projects")
"pp" '(consult-projectile-switch-project :which-key "Switch Project")
"pg" '(consult-rg :which-key "RipGrep")
"pf" '(consult-projectile-find-file :which-key "find file")
"pr" '(consult-projectile-recentf :which-key "recent files")
"pb" '(consult-projectile-switch-to-buffer :which-key "buffers")

;; Hydras
"H" '(nil :which-key "Hydras")
"Ht" '(rc-hydra-theme-switcher/body :which-key "themes")
"Hf" '(rc-hydra-variable-fonts/body :which-key "mixed-pitch face")
"Hw" '(rc-hydra-window/body :which-key "Window Control")
"Hd" '(hydra-dired/body :which-key "Dired")
"He" '(hydra-flycheck/body :which-key "Diagnostics")
"Ho" '(hydra-org-agenda/body :which-key "Org Agenda")
"Ha" '(hydra-avy/body :which-key "Avy")
"Hy" '(hydra-yasnippet :which-key "YASnippet")

;; Help/emacs
"h" '(nil :which-key "help/emacs")

"hv" '(helpful-variable :which-key "des. variable")
"hb" '(describe-bindings :which-key "des. bindings")
"hM" '(describe-mode :which-key "des. mode")
"hf" '(helpful-function :which-key "des. func")
"hF" '(describe-face :which-key "des. face")
"hk" '(helpful-key :which-key "des. key")

"hed" '((lambda () (interactive) (jump-to-register 67)) :which-key "edit dotfile")

"hm" '(nil :which-key "switch mode")
"hme" '(elisp-mode :which-key "elisp mode")
"hmo" '(org-mode :which-key "org mode")
"hmt" '(text-mode :which-key "text mode")

"hp" '(nil :which-key "packages")
"hpr" 'package-refresh-contents
"hpi" 'package-install
"hpd" 'package-delete

;; Windows
       "w" '(nil :which-key "window")
       "wm" '(rc/toggle-maximize-buffer :which-key "maximize buffer")
       "wN" '(make-frame :which-key "make frame")
       "wd" '(evil-window-delete :which-key "delete window")
       "ws" '(rc/split-window-vertically-and-switch :which-key "split below")
       "wv" '(rc/split-window-horizontally-and-switch :which-key "split right")
       "wr" '(rc-hydra-window/body :which-key "hydra window")
       "wl" '(evil-window-right :which-key "evil-window-right")
       "wh" '(evil-window-left :which-key "evil-window-left")
       "wj" '(evil-window-down :which-key "evil-window-down")
       "wk" '(evil-window-up :which-key "evil-window-up")
"wz" '(text-scale-adjust :which-key "text zoom")
"wu" '(winner-undo :which-key "Winnder undo")
"wU" '(winner-redo :which-key "Winner Redo")

"r" '(nil :which-key "Registers")
"rc" '(copy-to-register :which-key "Copy to Register")
"rf" '(frameset-to-register :which-key "Frameset to register")
"ri" '(insert-register :which-key "Insert contents of register")
"rj" '(jump-to-register :which-key "Jump to")
"rl" '(list-registers :which-key "List registers")
"rn" '(number-to-register :which-key "Number to register")
"rr" '(consult-register :which-key "Choose a register")
"rv" '(view-register :which-key "View register")
"rw" '(window-configuration-to-register :which-key "Window config to register")
"r+" '(increment-register :which-key "Increment register")
"r " '(point-to-register :which-key "Point to Register")
"rs" '(consult-register-store :which-key "Store")

"s" '(nil :which-key "Work[s]paces")
"ss" '(persp-switch :which-key "Switch")
"sn" '(persp-switch-by-number :which-key "Swich By #")
"sk" '(persp-remove-buffer :which-key "Remove Buffer")
"sd" '(persp-kill :which-key "Delete workspace")
"sr" '(persp-rename :which-key "Rename")
"sa" '(persp-add-buffer :which-key "Add a buffer")
"sc" '(persp-set-buffer :which-key "Claim buffer for perspective")
"sb" '(persp-switch-to-buffer :which-key "Switch to buffer (in any perspective)")
"sS" '(persp-state-save :which-key "Save")
"sL" '(persp-state-load :which-key "Load")

;; Toggles
"t" '(nil :which-key "toggles")
"tm" '(modus-themes-toggle :which-key "Modus Themes")
"ta" '(term-toggle-ansi :which-key "Ansi Term")
"tA" '(ansi-term :which-key "Ansi Term (HERE)")
"tt" '(term-toggle-term :which-key "Term")
"tT" '(term :which-key "Term (HERE)")
"te" '(term-toggle-eshell :which-key "Eshell")
"tE" '(eshell :which-key "Eshell (HERE)")
"td" '(dired-single :which-key "Dired")
"tp" '(treemacs :which-key "Treemacs")
"to" '(function :which-key "description")
)

(general-def
  :keymaps 'override
  :states '(normal motion)
  "K" 'eldoc
  "Q" 'popper-kill-latest-popup
)

(general-def
  :states '(normal visual motion)
  "gc" 'comment-dwim
  "gC" 'comment-line
  "u" 'undo-fu-only-undo
  "U" 'undo-fu-only-redo
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "f" 'evil-avy-goto-char-in-line
  )

(general-def
  :states '(normal visual motion)
  :keymaps 'override
  "s" 'swiper)

(general-def
  :states '(insert)
  "C-a" 'evil-beginning-of-visual-line
  "C-e" 'evil-end-of-visual-line
  "C-S-a" 'evil-beginning-of-line
  "C-S-e" 'evil-end-of-line
  "C-n" 'evil-next-visual-line
  "C-p" 'evil-previous-visual-line
  "TAB" 'yas-expand 

  )

(use-package emacs-lisp-mode
  :ensure nil
  :general
  (general-def
   :prefix ","
   :states 'motion
   :keymaps 'emacs-lisp-mode-map
   "" nil
   "e" '(nil :which-key "eval")
   "es" '(eval-last-sexp :which-key "eval-sexp")
   "er" '(eval-region :which-key "eval-region")
   "eb" '(eval-buffer :which-key "eval-buffer")

   "g" '(consult-imenu :which-key "imenu")
   "c" '(check-parens :which-key "check parens")
   "I" '(indent-region :which-key "indent-region")
   "b" '(nil :which-key "org src")
   "bc" '(org-edit-src-abort :which-key "Abort")
   "bb" '(org-edit-src-exit :which-key "Save & Exit")
   )


  )

(use-package dashboard
:straight (dashboard :fetcher github
	 :repo "emacs-dashboard/emacs-dashboard"
	 :files (:defaults "banners"))
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "")
(setq dashboard-startup-banner "~/.emacs.d/ka.png")

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))    )

(add-hook 'emacs-startup-hook (lambda ()
                                (when (get-buffer "*scratch*")
                                  (kill-buffer "*scratch*"))))
