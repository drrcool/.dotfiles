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

(use-package centered-cursor-mode
    :ensure t
  :config
(global-centered-cursor-mode +1))

(setq delete-by-moving-to-trash t
trash-directory "~/.local/share/Trash/files/")

(use-package beacon
  :ensure t
  :config
  (beacon-mode +1))

(setq org-confirm-babel-evaluate nil)

(use-package no-littering :ensure t)

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

(setq display-buffer-base-action
  '((display-buffer-reuse-window
     display-buffer-reuse-mode-window
     display-buffer-same-window
     display-buffer-in-previous-window)))

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
(require 'quelpa-use-package)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-hud nil)
  (setq doom-modeline-minor-modes nil)
  (setq rc-doom-modeline-text-height 200)
  (setq doom-modeline-height 35)
  )
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package modus-themes
:ensure t
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

(use-package doom-themes
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
  (doom-themes-org-config))

(use-package kaolin-themes
    :ensure t
    :config
    (setq kaolin-themes-modeline-border nil)
)

(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-headings
	(quote ((1 . (variable-pitch 1.1))
		(2 . (regular))
		(3 . (regular))
		(4 . (regular))))))

(load-theme 'doom-acario-dark t)

(set-face-attribute 'default nil :family "Anonymice Nerd Font Mono" :height 220)

 ;; Proportionally spaced typeface
 (set-face-attribute 'variable-pitch nil :family "OpenDyslexicAlta NF" :height 1.0)

 ;; Monospaced typeface
 (set-face-attribute 'fixed-pitch nil :family "Comic Sans MS" :height 1.5)


 (if (facep 'mode-line-active)
     (set-face-attribute 'mode-line-active nil :family "Spleen 32x64" :height 200) ; For 29+
   (set-face-attribute 'mode-line nil :family "Spleen 32x64" :height 200))
(set-face-attribute 'mode-line-inactive nil :family "Anonymice Nerd Font Mono" :height 200)

(use-package helpful
  :ensure t)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

(use-package popper
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
  :config
  (general-auto-unbind-keys)
  (general-evil-setup t))

(use-package ace-window :defer t :ensure t)

(use-package diminish :ensure t)

(use-package bufler
  :ensure t
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

(use-package avy :ensure t)

(use-package undo-fu :ensure t)

(use-package flycheck
:ensure t
  :config
(global-flycheck-mode))

(use-package exec-path-from-shell
:ensure t
:config
(exec-path-from-shell-initialize)):

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
  (evil-mode +1))
(use-package evil-collection
  :ensure t
  :after evil
:diminish
  :config
  (setq evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) calc diff-mode))
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-exchange
:ensure t
  :config
(evil-exchange-install)
)

(use-package evil-visualstar
:ensure t
  :config
(global-evil-visualstar-mode 1))

(use-package evil-dvorak :ensure t)

(use-package evil-escape
:ensure t
:straight (evil-escape :host github
:repo "syl20bnr/evil-escape"
:files (:defaults))
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
(with-eval-after-load 'company
(delq 'company-preview-if-just-one-frontend company-frontends))
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
)

(use-package yasnippet
 :ensure t
 :config
 (setq yas-snippet-dirs '("~/.doom.d/snippets"))
 (yas-global-mode 1))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
(setq which-key-idle-delay 0.01))

(use-package rainbow-delimiters
 :ensure t
:hook (prog-mode . rainbow-delimiters-mode))

(use-package hydra :ensure t)
(defhydra hydra-text-scale (:timeout 4)
          "scale text"
          ("j" text-scale-increase "in")
          ("k" text-scale-decrease "out")
          ("f" nil "finished" :exit t))

(use-package quickrun
   :defer t
   :ensure t
   :general
   (general-define-key
    :states 'normal
    :prefix "SPC"
    :keymaps 'quickrun--mode-map
    "cq" '(nill :which-key "quickrun")
    "cqq" '(quit-window :which-key "Quit")
    "cqr" '(quickrun :which-key "Run")
    "cqR" '(quickrun-region :which-key "Run Region")
    "cqa" '(quickrun-with-arg :which-key "Run with [A]rgs")
    "cqm" '(quickrun-autorun-mode :which-key "Toggle autorun mode")
    "cqs" '(quickrun-select :which-key "Select backend")"cq" '(nill :which-key "quickrun")
    "cqq" '(quit-window :which-key "Quit")
    "cqr" '(quickrun :which-key "Run")
    "cqR" '(quickrun-region :which-key "Run Region")
    "cqa" '(quickrun-with-arg :which-key "Run with [A]rgs")
    "cqm" '(quickrun-autorun-mode :which-key "Toggle autorun mode")
    "cqs" '(quickrun-select :which-key "Select backend")
))

(use-package rainbow-mode :ensure t :diminish)
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

(use-package projectile
  :ensure t
:diminish
  )

(use-package counsel-projectile
  :ensure t
  :after projectile
:diminish
  :init
  (counsel-projectile-mode +1)
  )

(use-package perspective
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

(setq org-agenda-scheduled-leaders '("Plan | " "Sched.%2dx: ") ; ???
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

  "b" '(nill :which-key "org src")
  "bb" 'org-edit-src-exit
  "bc" 'org-edit-src-abort)

(general-define-key
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
 "g" '(counsel-org-goto :which-key "goto heading")
 "t" '(counsel-org-tag :which-key "set tags")
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
 "iL" '(counsel-org-link :which-key "counsel-org-link")
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
(general-define-key
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
  :defer t
:diminish
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; (use-package org-category-capture :ensure t)
;; (use-package org-projectile
;;   :after org org-category-capture
;;   :config
;;   (progn
;;     (setq org-projectile-projects-file
;;           "~/org/projects.org"
;;           org-projectile-capture-template
;;           ("* TODO %?\n %i\n %a")
;;           )
;;     (add-to-list 'org-capture-templates
;;                  (org-projectile-project-todo-entry
;;                   :capture-character "l"
;;                   :capture-heading "Linked Project TODO"))
;;     (add-to-list 'org-capture-templates
;;                  (org-projectile-project-todo-entry
;;                   :capture-character "p"))

;;     (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;;     (push (org-projectile-project-todo-entry) org-capture-templates))
;;   :ensure t

;;   )

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx))
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package eglot :ensure t)

(use-package lsp-mode
  :ensure t
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

(use-package tsi
:ensure t
      :after tree-sitter
      :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
      ;; define autoload definitions which when actually invoked will cause package to be loaded
      :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
      :init
      (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
      (add-hook 'tsx-ts-mode-hook (lambda () (tsi-typescript-mode 1)))
      (add-hook 'js-mode-hook (lambda () (tsi-typescript-mode 1)))
      (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
      (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
      (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package apheleia
  :ensure t
:diminish
  :config
  (apheleia-global-mode +1))

(use-package lua-mode
  :ensure t
  :after (tree-sitter lsp-mode)
:init
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
)

(use-package company
  :ensure t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-ui
    :ensure t
    :hook ((lsp-mode . lsp-ui-mode)
(lsp-mode . lsp-ui-sideline-mode))
    :config
(setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-position 'top))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-position 'right
        width 50))

(use-package lsp-treemacs
  :ensure t
:after lsp)

(use-package treemacs-evil
  :ensure t
  :config
  )

(use-package treemacs-projectile
  :ensure t
  )

(use-package treemacs-magit
  :ensure t
  )

(use-package treemacs-icons-dired :ensure t)

(use-package treemacs-perspective :ensure t)

(use-package treemacs-all-the-icons :ensure t)

(use-package ivy
  :ensure t
  :diminish 
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  )

(use-package prescient
  :after counsel
    :ensure t)
(use-package ivy-prescient :ensure t
  :config
(ivy-prescient-mode))

(use-package lsp-ivy
  :ensure t)

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
 :config
 (setq ivy-format-function #'ivy-format-function-line)
 (setq ivy-rich-display-transformers-list
       (plist-put ivy-rich-display-transformers-list
                  'ivy-switch-buffer
                  '(:columns
                    ((ivy-rich-candidate (:width 40))
                     (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                     (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                     (ivy-rich-switch-buffer-project (:width 15 :face success))
                     (ivy-rich-switch-buffer-path (:width (lambda(x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                    :predicate
                    (lambda (cand)
                      (if-let ((buffer (get-buffer cand)))
                          (with-current-buffer buffer
                            (not (derived-mode-p 'exwm-mode)))))))))

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x . ivy-display-function-fallback)
          (counsel-esh-history . ivy-posframe-display-at-window-center)
          (counsel-describe-function . ivy-display-function-fallback)
          (counsel-describe-variable . ivy-display-function-fallback)
          (counsel-find-file . ivy-posframe-display-at-window-center)
          (counsel-recentf . ivy-posframe-display-at-window-center)
          (counsel-register . ivy-posframe-display-at-frame-bottom-window-center)
          (dmenu . ivy-posframe-display-at-frame-top-center)
          (nil . ivy-posframe-display))
        ivy-posframe-height-alist
        '((swiper . 20)
          (dmenu . 20)
          (t . 10)))
  (ivy-posframe-mode +1)
  )

(use-package magit
:ensure t
:commands (magit-status magit-get-current-branch)
:custom
(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :ensure t
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package mixed-pitch
  :ensure t
  :defer t
  :config
  (setq mixed-pitch-set-height nil)
  (dolist (face '(org-date org-priority org-tag org-special-keyword))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
 )

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
           ("1" (load-theme 'doom-one t)             "one")
           ("2" (load-theme 'modus-vivendi t)             "modus-vivendi")
           ("3" (load-theme 'doom-molokai t)             "molokai")
           ("4" (load-theme 'doom-snazzy t )             "snazzy")
           ("5" (load-theme 'doom-old-hope t)             "old hope")
           ("6" (load-theme 'doom-henna t)             "henna")
           ("7" (load-theme 'kaolin-galaxy t)             "jaolin-galaxy")
           ("8" (load-theme 'doom-monokai-machine t)             "monokai-machine")
           ("9" (load-theme 'doom-xcode t)             "xcode")
           ("0" (load-theme 'doom-moonlight t)             "moonlight")
           ("-" (load-theme 'doom-laserwave t)             "laserwave")
           ("z" (load-theme 'doom-one-light t)
            "one-light")
           ("x" (load-theme 'modus-operandi t)             "operand")
("c" (load-theme 'ef-trio-light t)             "ef-trio-light")
("v" (load-theme 'ef-dark t)             "ef-dark")
("b" (load-theme 'doom-dark+ t)             "doom-dark+")
("n" (load-theme 'doom-Iosvkem t)             "Iosvkem")
("m" (load-theme 'doom-vibrant t)             "vibrant")
 ("q" nil)

           )

(defhydra rc-hydra-window (:hint nil)
   "
Movement      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------------
_M-<left>_  <   _s_ vertical      _b_uffer        _<left>_  <
_M-<right>_ >   _v_ horizontal    _f_ind file     _<down>_  ???
_M-<up>_    ???   _m_aximize        _x_wap          _<up>_    ???
_M-<down>_  ???   _c_lose           _[_backward     _<right>_ >
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
   ("b" counsel-switch-buffer)
   ("f" counsel-find-file)
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

(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"

 ;; Top level functions
 "/" '(rc/rg :which-key "RipGrep")
 "SPC" '(counsel-M-x :which-key "M-x")
 "q" '(popper-kill-latest-popup :which-key "kill popup")
 "X" '(org-capture :which-key "Capture")

"f" '(nil :which-key "Files")
"fd" '(counsel-dired-jump :which-key "Find directory")
"ff" '(counsel-find-file :which-key "Find file")
"fl" '(counsel-locate :which-key "Locate")
"fr" '(counsel-recentf :which-key "Recent Files")
"fR" '(counsel-find-file-move :which-key "Move File")
"fs" '(save :which-key "Save")

;; Buffers
"b" '(nil :which-key "Buffer")
"bb" '(persp-counsel-switch-buffer :which-key "Switch buffer")
"bd" '(evil-delete-buffer :which-key "Delete buffer")
"bm" '(rc/kill-other-buffers :which-key "Kill other buffers")
"bi" '(ibuffer :which-key "iBuffer")
"br" '(revert-buffer :which-key "Revert buffer")
"bn" '(next-buffer :which-key "Next")
"bp" '(previous-buffer :which-key "Prev")

;; lCode

"c" '(nil :which-key "Code")
"cd" '(lsp-ui-peek-find-definitions :which-key "Definition")
"cR" '(lsp-ui-peek-find-references :which-key "References")
"ca" '(lsp-execute-code-action :which-key "Code action")
"ci" '(lsp-ui-peek-find-implementation :which-key "Implementations")
"cD" '(lsp-goto-type-definition :which-key "Type Def.")
"cc" '(lsp-treemacs-call-hierarchy :which-key "Call hierarchy")
"ct" '(lsp-treemacs-type-hierarchy :which-key "Type hierarchy")
"cs" '(lsp-treemacs-symbols :which-key "Symbol Hierarchy")
"cT" '(lsp-treemacs-tree :which-key "Tree")
"ce" '(nil :which-key "Errors")
"cek" '(flycheck-previous-error :which-key "Prev Error")
"cej" '(flycheck-next-error :which-key "Next Error")
"ceg" '(flycheck-first-error :which-key "First Error")
"cef" '(flycheck-error-list-set-filter :which-key "Filter")
"ceG" '((progn (goto-char (point-max)) (flycheck-previour-error)) :which-key "Last Error")
"cel" '(counsel-flycheck :which-key "Error List")
"cee" '(flycheck-explain-error-at-point :which-key "Explain error")

"cf" '(nil :which-key "Format")
"cfl" '(lsp-format-buffer :which-key "LSP format Buffer")
"cfa" '(apheleia-format-buffer :which-key "Apheleia Format")
"cfr" '(lsp-format-region :which-key "LSP Format Region")
"cft" '(lsp-on-type-formatting :which-key "Toggle On Type Formatting")

"m" '(nil :which-key "Bookmarks")
"ma" '(bookmark-set  :which-key "Set a bookmark")
"mj" '(bookmark-jump-other-window :which-key "Jump to bookmark")
"mJ" '(bookmark-jump :which-key "Jump Here")
"mb" '(counsel-bookmark :which-key "Set or Jump")
"md" '(counsel-bookmark-avoid-dired :which-key "Jump to Directory")

;;Files.
"f" '(nil :which-key "files")
"fb" '(counsel-bookmark :which-key "bookmarks")
"ff" '(counsel-find-file :which-key "find file")
"fr" '(counsel-recentf :which-key "recent files")
"fR" '(rename-file :which-key "rename file")
"fs" '(save-buffer :which-key "save buffer")
"fS" '(evil-write-all :which-key "save all buffers")

"p" '(nil :which-key "Projects")
"pp" '(counsel-projectile-switch-project :which-key "Switch Project")
"pf" '(counsel-projectile-find-file :which-key "Files")
"pd" '(counsel-projectile-find-dir :which-key "Directories")
"pb" '(counsel-projectile-switch-to-buffer :which-key "Buffers")
"pg" '(counsel-projectile-rg :which-key "RipGrep")
 "pX" '(counsel-projectile-org-capture :which-key "Capture")
"pa" '(counsel-projectile-org-agenda :which-key "Agenda")

;; Hydras
"H" '(nil :which-key "Hydras")
"Hs" '(hydra-text-scale/body :which-key "Scale text")
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
"hb" '(counsel-descbinds :which-key "des. bindings")
"hM" '(describe-mode :which-key "des. mode")
"hf" '(helpful-function :which-key "des. func")
"hF" '(counsel-describe-face :which-key "des. face")
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
"rr" '(counsel-register :which-key "Choose a register")
"rv" '(view-register :which-key "View register")
"rw" '(window-configuration-to-register :which-key "Window config to register")
"r+" '(increment-register :which-key "Increment register")
"r " '(point-to-register :which-key "Point to Register")

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
"td" '(dired :which-key "Dired")
"tp" '(treemacs :which-key "Treemacs")
) ;; End SPC prefix block

(general-def
  :keymaps 'override

  ;; Emacs
  "M-x" 'counsel-M-x
  )
(general-def
  :keymaps 'override
  :states '(normal motion)
  "K" 'lsp-ui-doc-show
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

;; (use-package web-mode
;;   :ensure t
;;   :defer t
;; :after lsp-mode
;;   :config
;;   (setq web-mode-enable-current-element-highlight t
;;         web-mode-enable-current-column-highlight t
;;         web-mode-enable-auto-quoting nil
;;         web-mode-markup-indent-offset 2
;;         web-mode-attr-indent-offset 2
;;         web-mode-attr-value-indent-offset 2)

;;   (define-derived-mode web-typescript-mode web-mode "TS [web]")
;;   (define-derived-mode web-js-mode web-mode "JS [web]")
;;   (define-derived-mode web-html-mode web-mode "HTML [web]")
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . web-mode))

;;   :general
;;   (general-def
;;     :prefix ","
;;     :states 'motion
;;     :keymaps 'web-mode-map
;;     "" nil
;;     "i" '(web-mode-buffer-indent :which-key "Web mode indent")
;;     "c" '(web-mode-fold-or-unfold :which-key "Web mode toggle fold")
;;     ))

;; (use-package json-mode :ensure t)       ;

(use-package emacs-lisp-mode
  :ensure nil
  :general
  (general-define-key
   :prefix ","
   :states 'motion
   :keymaps 'emacs-lisp-mode-map
   "" nil
   "e" '(nil :which-key "eval")
   "es" '(eval-last-sexp :which-key "eval-sexp")
   "er" '(eval-region :which-key "eval-region")
   "eb" '(eval-buffer :which-key "eval-buffer")

   "g" '(counsel-imenu :which-key "imenu")
   "c" '(check-parens :which-key "check parens")
   "I" '(indent-region :which-key "indent-region")
   "b" '(nil :which-key "org src")
   "bc" '(org-edit-src-abort :which-key "Abort")
   "bb" '(org-edit-src-exit :which-key "Save & Exit")
   )


  )

(defun tide-setup-hook()
  (tide-setup)
  (eldoc-setup)
  (flycheck-set-indication-mode +1)
  (tide-hl-identifier-mode +1)
  (set (make-local-variable 'company-backends)
       '((company-tide company-files :with company-yasnippet
                       company-dabbrev-code company-dabbrev))))

(use-package tide
  :ensure t
  :after (company flycheck)
  :hook (
         (before-save . tide-format-before-save)
         (tsx-ts-mode-hook . tide-setup-hook)
         (js-mode-hook . tide-setup-hook)
         (typescript-mode-hook . tide-setup-hook)
         )
 :config
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
 (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)



:general
(general-def
  :prefix "SPC"
:states 'normal
:keymap: 'override

  "j"  '(nil :whick-key "Tide (TS/JS)")
  "jr" '(tide-refactor :which-key "Refactor")
  "jo" '(tide-organize-imports :which-key "Organize Imports")
  "jR" '(tide-rename-file :which-key "Rename the files and references")
  "js" '(tide-rename-symbol :which-key "Rename symbol")
  "je" '(tide-error-at-point :which-key "Error details at point")

    ))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "")
(setq dashboard-startup-banner "~/.emacs.d/ka.png")

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))    )

(add-hook 'emacs-startup-hook (lambda ()
                                (when (get-buffer "*scratch*")
                                  (kill-buffer "*scratch*"))))
