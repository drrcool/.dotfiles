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
:states '(normal motion visual)
:prefix "SPC")

(rcool/leader-keys
"" nil 
))

(use-package which-key
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.3
        which-key-prefix-prefix "+"
        which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 3
        which-key-max-display-columns nil))

(use-package evil
  :init
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
(evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

(use-package company-box
  :diminish
  :functions (all-the-icons-favicon
              all-the-icons-material
              all-the-icons-octicon
              all-the-icons-alltheicon)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-enable-icon (display-graphic-p))
  :config
  (setq company-box-backend-colors nil)
 )

(use-package projectile
  :diminish
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :general
  (rcool/leader-keys
    "p!" '(projectile-run-shell-command-in-root :wk "Run cmd in root")
    "p&" '(projectile-run-async-shell-command-in-root :wk "Async cmd in root")
    "pa" '(projectile-add-known-project :wk "Add new project")
    "pb" '(projectile-switch-to-buffer :wk "Switch to project buffer")
    "pd" '(projectile-remove-known-project :wk "Remove project")
    "pf" '(projectile-find-file :wk "Find Project File")
    "pg" '(projecfile-configure-project :wk "Configure project")
    "pk" '(projectile-kill-buffers :wk "Kill project buffers")
    "po" '(projectile-find-other-file :wk "Find Other File")
    "pp" '(projectile-switch-project :wk "Switch projects")
    "pr" '(projectile-recentf :wk "Find Recent Project Files")
    "ps" '(projectile-save-project-buffers :wk "Save Project Buffers")
    "pt" '(magit-todos-list :wk "List project todos"))

  :init
  (when (file-directory-p "~/Documents/projects")
    (setq projectile-project-search-path '("~/Documents/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package perspective

  :custom
  (persp-mode-prefix-key (kbd "M-p"))
  :init
  (persp-mode)

  :general
  (rcool/leader-keys
    "W" '(:ignore t :wk "Workspaces")
    "W." '(persp-switch :wk "Switch Workspace")
    "Ws" '(persp-state-save :wk "Save")
    "Wl" '(persp-state-load :wk "Load")
    "Wn" '(persp-next :wk "Next")
    "Wp" '(persp-prev :wk "Prev")
    "Wr" '(persp-rename :wk "Rename")
    "Wn" '(persp-switch-by-number :wk "Switch to Number"))
  )

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

(use-package org-menu
 :after org
 :commands (org-menu)
 :general
 (rcool/leader-keys
 "om" '(org-menu :wk "Org Menu") )
)

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil)
  (add-hook 'org-agenda-mode-hook
			#'(lambda () (setq-local nobreak-char-display nil)))
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

  :general
  (rcool/leader-keys
    "n" '(:ignore t :wk "Notes")
    "nd" '(:ignore t :wk "By date")
    "nd-" '(org-roam-dailies-find-directory :wk "Find Directory")
    "ndy" '(org-roam-dailies-goto-yesterday :wk "Goto Yesterday")
    "ndT" '(org-roam-dailies-capture-today :wk "Capture Today")
    "ndt" '(org-roam-dailies-goto-today :wk "Goto Today")
    "ndY" '(org-roam-dailies-capture-yesterday :wk "Capture Yesterday")
    "ndf" '(org-roam-dailies-goto-next-note :wk "Next Note")
    "ndd" '(org-roam-dailies-goto-date :wk "Goto Date")
    "nf" '(org-roam-node-find :wk "Find")
    "ni" '(org-roam-node-insert :wk "Insert")
    "no" '(org-roam-node-open :wk "Open")
    "nn" '(org-roam-capture :wk "Capture to node")
    "ng" '(org-roam-graph :wk "Show graph")
    "nF" '(org-roam-ref-find :wk "Find Ref")
    "ns" '(org-roam-db-sync :wk "Sync database")


    )

 )

(defun rcool-buffer-tags-get ()
  "Return filetags value in current buffer."
  (rcool-buffer-prop-get-list "filetags" " "))

(defun rcool-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.

If SEPARATORS is non-nil, it should be a regular expression matching text
that separates, but is not part of, the substrings.  If nil, it defaults
to `split-string-default-separators'."
  (let ((value (rcool-buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

(defun rcool-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ":\\(.*\\)$") (point-max) t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun rcool-buffer-tags-add (tag)
  "Add a TAG to filetags in current buffer."
  (let* ((tags (rcool-buffer-tags-get))
         (tags (delete tag tags)))
    (apply #'rcool-buffer-tags-set tags)))

(defun rcool-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.

If filetags value is already set, replace it."
  (rcool-buffer-prop-set "filetags" (string-join tags " ")  ))

(defun rcool-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.

If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)$") (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(add-hook 'find-file-hook #'rcool-project-update-tag)
(add-hook 'before-save-hook #'rcool-project-update-tag)

(defun rcool-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (rcool-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (rcool-buffer-tags-get))
             (original-tags tags))
        (if (rcool-project-p)
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))

        ;; Clean up dups
        (setq tags (seq-uniq tags))

        ;; update tags
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'rcool-buffer-tags-set tags))))))

(defun rcool-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))

        (file-name-directory buffer-file-name))))


(defun rcool-project-p ()
  "Return non-nil if current buffer has any todo entries.

TODO entriest marked as done are ignored, meaning that this function
returns nil if current buffer contains only completed tasks."
  (org-element-map
               (org-element-parse-buffer 'headline)
               'headline
               (lambda (h)
                 (eq (org-element-property :todo-type h)
                     'todo))
               nil 'first-match))

(defun rcool-project-files ()
  "Return a list of note files containing 'project' tags."

  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (like tag (quote "%\"project\"%"))]))))


(defun rcool-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files',"
  (setq org-agenda-files (rcool-project-files)))

(advice-add 'org-agenda :before #'rcool-agenda-files-update)
(advice-add 'org-todo-list :before #'rcool-agenda-files-update)

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
  )

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "M-[" 'org-metaleft
  "M-]" 'org-metaright
  )

;; Org-src
(rcool/leader-keys
  "o" '(:ignore t :wk "Org")
  "ob" '(:ignore t :wk "Code Block")
  "obk" '(org-edit-src-abort :wk "Abort")
  "obc" '(org-edit-src-exit :wk "Accept & Exit")
  "obe" '(org-edit-special :wk "Edit")
  "oc" '(org-ctrl-c-ctrl-c :wk "C-c C-c") 
  )

(general-def
  :keymaps 'org-src-mode-map
  "C-c C-c" 'org-edit-src-exit)

(setq rcool/default-line-spacing 1)
(setq-default line-spacing rcool/default-line-spacing)
(defun rcool/org-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (centered-cursor-mode)
  (setq-local line-spacing (+ rcool/default-line-spacing 2))
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

  :general
  (rcool/leader-keys
    "oA" '(org-archive-subtree-default :wk "Archive")
    "oa" '(org-agenda :wk  "Agenda")
    "o6" '(org-sort :wk "Sort")
    "oc" '(org-capture :wk "Capture")
    "os" '(org-schedule :wk "Schedule")
    "od" '(org-deadline :wk "Deadline")
    "og" '(counsel-org-goto :wk "Goto Heading")
    "ot" '(counsel-org-tag :wk "Set Tags")
    "oT" '(org-todo :wk "Todo States")
    "op" '(org-set-priority :wk "Set Priority")
    "or" '(org-refile :wk "Refile")
    "oe" '(org-export-dispatch :wk "Export")
    "o." '(org-narrow-to-subtree :wk "Narrow to Subtree")
    "o1" '(org-toggle-link-display :wk "Toggle Link Display")
    "o2" '(org-toggle-inline-images :wk "Toggle  Images")

    "oi" '(:ignore t :wk "Insert")
    "oil" '(org-insert-link :wk "Insert Link")
    "oiL" '(counsel-org-link :wk "Insert Link (Counsel)")

    "c" '(:ignore t :wk "Clock")
    "ci" '(org-clock-in :wk "Clock In")
    "co" '(org-clock-out :wk "Clock Out")
    "cj" '(org-clock-goto :wk "Clock Goto")

    )

  (general-define-key
   :prefix ","
   :states 'motion
   :keymaps '(org-agenda-mode-map)
   "" nil
   "a" '(org-agenda :which-key "org agenda")
   "c" '(org-capture :which-key "org-capture")
   "s" '(org-agenda-schedule :which-key "schedule")
   "," '(org-agenda-schedule :which-key "schedule") ;; quick access
   "d" '(org-agenda-deadline :which-key "deadline")
   "t" '(org-agenda-set-tags :which-key "set tags")
   ;; clocking
   "c" '(nil :which-key "clocking")
   "ci" '(org-agenda-clock-in :which-key "clock in")
   "co" '(org-agenda-clock-out :which-key "clock out")
   "cj" '(org-clock-goto :which-key "jump to clock")
   )

  )

(setq org-ellipsis "⤵")
;; ⤵ ▼ ⬎  
(setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
(setq org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX
(setq org-startup-folded 'showeverything)
(setq org-image-actual-width 300)
(setq org-fontify-whole-heading-line t)

(setq org-cycle-separator-lines 1)
(setq org-catch-invisible-edits 'show-and-error) ;; 'smart
(setq org-src-tab-acts-natively t)

;; M-Ret can split lines on items and tables but not headlines and not on anything else (unconfigured)
(setq org-M-RET-may-split-line '((headline) (item . t) (table . t) (default)))
(setq org-loop-over-headlines-in-active-region nil)

;; Opens links to other org file in same frame (rather than splitting)
(setq org-link-frame-setup '((file . find-file)))

(setq org-log-done t
      org-log-into-drawer t)

;; Automatically change bullet type when indenting
;; Ex: indenting a + makes the bullet a *.
(setq org-list-demote-modify-bullet
      '(("+" . "*") ("*" . "-") ("-" . "+")))

;; Automatically save and close the org files I most frequently archive to.
;; I see no need to keep them open and crowding my buffer list.
;; Uses my own function jib/save-and-close-this-buffer.
(dolist (file '("homework-archive.org_archive" "todo-archive.org_archive"))
  (advice-add 'org-archive-subtree-default :after 
              (lambda () (jib/save-and-close-this-buffer file))))

(defun rcool/post-org-goto ()
  (let ((current-prefix-arg '(4))) ;; emulate C-u
    (call-interactively 'org-reveal))
  (org-cycle))

(advice-add 'counsel-org-goto :after #'rcool/post-org-goto)
(advice-add 'org-agenda-goto :after #'rcool/post-org-goto)
(advice-add 'org-agenda-switch-to :after #'rcool/post-org-goto)

(setq org-todo-keywords '((type
                           "TODO(t)" "WAITING(h)" "INPROG-TODO(i)" "WORK(w)"
                           "STUDY(s)" "SOMEDAY" "READ(r)" "PROJ(p)" "CONTACT(c)"
                           "AUDIO(a)" "VIDEO(v)"
                           "|" "DONE(d)" "CANCELLED(C@)")))

(setq org-todo-keyword-faces
      '(("TODO"  :inherit (region org-todo) :foreground "DarkOrange1"   :weight bold)
        ("WORK"  :inherit (org-todo region) :foreground "DarkOrange1"   :weight bold)
        ("READ"  :inherit (org-todo region) :foreground "MediumPurple2" :weight bold)
        ("VIDEO"  :inherit (org-todo region) :foreground "MediumPurple2" :weight bold)
        ("AUDIO"  :inherit (org-todo region) :foreground "MediumPurple2" :weight bold)
        ("PROJ"  :inherit (org-todo region) :foreground "orange3"     :weight bold)
        ("STUDY" :inherit (region org-todo) :foreground "plum3"       :weight bold)
        ("DONE" . "SeaGreen4")))

(setq org-tags-column -1)

(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 . "red2")
        (66 . "Gold1")
        (67 . "Goldenrod2")
        (68 . "PaleTurquoise3")
        (69 . "DarkSlateGray4")
        (70 . "PaleTurquoise4")))

;; Org-Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
	 (python . t)
	 (shell . t)
	 (gnuplot . t)
	 (java . t)
(js . t)
  (lua . t)
(sql . t)
	 ))

  (use-package gnuplot :defer t)

  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  (setq python-shell-completion-native-enable nil)

  ;; How to open buffer when calling `org-edit-special'.
  (setq org-src-window-setup 'current-window)

;; custom time stamp format. I don't use this.
(setq org-time-stamp-custom-formats '("<%A, %B %d, %Y" . "<%m/%d/%y %a %I:%M %p>"))

(setq org-agenda-restore-windows-after-quit t)

(setq org-agenda-window-setup 'current-window)

;; Only show upcoming deadlines for the next X days. By default it shows
;; 14 days into the future, which seems excessive.
(setq org-deadline-warning-days 3)
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

(setq org-agenda-block-separator nil)

(setq org-agenda-scheduled-leaders '("Plan | " "Sched.%2dx: ") ; ⇛
      org-agenda-deadline-leaders '("Due: " "Due in %1d d. | " "Due %1d d. ago: "))

(setq org-agenda-prefix-format '((agenda . "  %-6:T %t%s")
                                 (todo . "  %-6:T %t%s")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (setq-local line-spacing 6)))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (hide-mode-line-mode)))

(setq org-agenda-custom-commands nil)
(add-to-list '
 org-agenda-custom-commands
 '("c" "Day View"
   ((agenda "" ((org-agenda-overriding-header "Productivity View")
                (org-agenda-span 'day)
                (org-super-agenda-groups '(
                                           (:name "Today's Tasks:"
                                                  :scheduled t
                                                  :order 2)
                                           (:name "Unscheduled Tasks Due Soon:"
                                                  :deadline t
                                                  :order 3)
                                           (:name "Today's Schedule:"
                                                  :time-grid t
                                                  :discard (:deadline t)
                                                  :order 1)))))

    ;; (org-ql-block '(and (not (tags "defer")) (or (todo "PROJ" "STUDY") (and (todo) (or (tags "ec" "lt") (tags "p")))))
    ;;               ((org-ql-block-header "")
    ;;                (org-super-agenda-groups '(
    ;;                                           (:name "Extracurricular:"
    ;;                                                  :tag "ec"
    ;;                                                  :order 5)
    ;;                                           (:name "Personal:"
    ;;                                                  :tag "p"
    ;;                                                  :order 10)
    ;;                                           (:name "Long-Term:"
    ;;                                                  :todo ("STUDY" "PROJ")
    ;;                                                  :tag "lt")
    ;;                                           (:discard (:todo t))))))

    ;; (todo "TODO"
    ;; 		(
    ;; 		 ;;(org-agenda-prefix-format "[ ] %T: ")
    ;; 		 (org-agenda-sorting-strategy '(tag-up priority-down))
    ;; 		 ;; (org-agenda-todo-keyword-format "")
    ;; 		 (org-agenda-overriding-header "\n Todos: ")))
    ;; (todo "PROJ"
    ;; 		((org-agenda-overriding-header "")))

    (alltodo "" ((org-agenda-overriding-header "")
             ;; (org-agenda-prefix-format "  %-6:T   ")
                 ;; (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-super-agenda-groups
                  '(
                    (:discard (:tag "defer"))
                    (:name "Extracurricular:"
                           :tag "ec"
                           :order 5)
                    (:name "Personal:"
                           :tag "p"
                           :order 10)
                    (:name "Study:"
                           :todo "STUDY")
                    (:name "Projects:"
                           :todo "PROJ")
                    (:discard (:todo t))
                    ))))

    )))

(add-to-list 'org-agenda-custom-commands
             '("v" "Day View No Agenda"
               ((org-ql-block '(todo)
                              ((org-super-agenda-groups '((:name "Today's Tasks"
                                                                 :scheduled today
                                                                 :deadline today)
                                                          (:discard (:tag "defer"))
                                                          (:name "Extracurricular:"
                                                                 :tag "ec"
                                                                 :order 10)
                                                          (:name "Personal:"
                                                                 :tag "p"
                                                                 :order 5)
                                                          (:name "Projects"
                                                                 :todo ("STUDY" "PROJ")
                                                                 :tag "lt")
                                                          (:discard (:todo t)))))))))

(add-to-list 'org-agenda-custom-commands
             '("w" "Six-Day View"
               ((agenda ""
                        ((org-agenda-span 6)
                         (org-agenda-entry-types '(:deadline :scheduled))
                         (org-agenda-start-on-weekday nil)
                         (org-deadline-warning-days 0)))
                ;; (todo "PROJ"
                ;; 	  (
                ;; 	   ;; (org-agenda-skip-function
                ;; 	   ;; 	'(org-agenda-skip-entry-if 'deadline))
                ;; 	   (org-agenda-prefix-format "%s ")
                ;; 	   (org-agenda-overriding-header "\Long-term:")))
                (org-ql-block '(and (not (tags "defer")) (or (todo "PROJ" "STUDY") (and (todo) (or (tags "ec" "lt") (tags "p")))))
                              ((org-ql-block-header "")
                               (org-super-agenda-groups '(
                                                          (:name "Extracurricular:"
                                                                 :tag "ec"
                                                                 :order 5)
                                                          (:name "Personal:"
                                                                 :tag "p"
                                                                 :order 10)
                                                          (:name "Long-Term:"
                                                                 :todo ("STUDY" "PROJ")
                                                                 :tag "lt")
                                                          (:discard (:todo t))))))


                )))



(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
)

(defun rcool/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
	  (indent-for-tab-command)))

(with-eval-after-load 'copilot
(evil-define-key 'insert copilot-mode-map
(kbd "<tab>") #'rcool/copilot-tab))

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(rcool/leader-keys
"f" '(:ignore t :wk "Files")
  "ff" '(find-file :wk "Find File")
  "fr" '(recentf-open-files :wk "Recent Files")
  "fs" '(save-buffer :wk "Save")
  )

(rcool/leader-keys
"b" '(:ignore t :wk "Buffer")
  "bb" '(counsel-switch-buffer :wk "Switch")
  "bd" '(evil-delete-buffer :wk "Delete")
"br" '(revert-buffer :wk "Revert")
"bn" '(evil-next-buffer :wk "Next")
"bp" '(evil-prev-buffer :wk "Previous")
  )

(use-package helpful
:custom
(counsel-describe-function-function #'helpful-callable)
(counsel-describe-variable-function #'helpful-variable)
:bind
([remap describe-function] . counsel-describe-function)
([remap describe-commpand] . helpful-command)
([remap describe-variable] . counsel-describe-variable)
([remap describe-key] . helpful-key))

(rcool/leader-keys
  "h'" 'describe-char
  "ha" 'apropos
  "hc" 'describe-key-briefly
  "hf" 'describe-function
  "hF" 'describe-face
  "hk" 'describe-key
  "hm" 'describe-mode
  "ho" 'describe-symbol
  "hv" 'describe-variable
  "ht" 'counsel-load-theme
  "hx" 'describe-command
  "hb" '(:ignore t :wk "Bindings")
  "hbb" 'describe-bindings
  "hbf" 'which-key-show-full-keymap
  "hbi" 'which-key-show-minor-mode-keymap
  "hbk" 'which-key-show-keymap
  "hbm" 'which-key-show-major-mode
  "hbt" 'which-key-show-top-level)

(use-package ace-window
:general
  (rcool/leader-keys
"w" '(:ignore t :wk "Window")
	"w=" '(balance-windows :wk "Balance")
	"wa" '(ace-window :wk "Ace Window")
	"wd" '(ace-delete-window :wk "Delete")
	"wh" '(evil-window-left :wk "Left Window")
	"wj" '(evil-window-down :wk "Down Window")
	"wk" '(evil-window-up :wk "Up Window")
	"wl" '(evil-window-right :wk "Right Window")
	"ws" '(evil-window-split :wk "Split")
	"wS" '(ace-swap-window :wk "Swap")
	"wu" '(winner-undo :wk "Winner Undo")
	"wv" '(evil-window-vsplit :wk "Vsplit")
))

(use-package ivy
  :diminish
  :config
  (setq ivy-extra-directories nil
		ivy-initial-inputs-alist nil
		ivy-fixed-height-minibuffer t)
  (setq-default ivy-height 10)
  (ivy-mode 1)

:general
(rcool/leader-keys
  "s" '(:ignore t :which-key "Search")
  "ss" '(swiper :which-key "Swiper")
  ))

(use-package ivy-rich
  :after counsel
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :init
  (setq ivy-postframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
		'((left-fringe . 8)
		  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package counsel

  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)

  :config
  (counsel-mode 1)
  :general
  (rcool/leader-keys
	":" '(counsel-M-x :which-key "M-x")
	"bi" '(counsel-ibuffer :which-key "ibuffer")
	)
  )

(use-package smex
  :defer 1
  :after counsel)

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :general
  (rcool/leader-keys
    "t" '(:ignore t :wk "Toggles")
    "tt" '(treemacs :wk "Treemacs")
    )
  )

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  )

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-perspective
  :after (treemacs perspective)
  :config (treemacs-set-scope-type 'Perspectives))

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :custom
  ((dired-listing-switches "-agho --group-directories-first")
   (insert-directory-program "/opt/homebrew/bin/gls")
   (delete-by-moving-to-trash t))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
                              "h" 'dired-single-up-directory
                              "l" 'dired-single-buffer)
  :general
  (rcool/leader-keys
    "t-" '(dired :wk "Dired")
    )
  )

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package bufler
    :ensure t
    :general
    (rcool/leader-keys
      "bb" '(bufler-switch-buffer :wk "Bufler Switch")
      "bw" '(bufler-workspace-frame-set :wk "Workspace Buffers"))
    :config
    (evil-collection-define-key 'normal 'bufler-list-mode-map
      (kbd "RET") 'bufler-list-buffer-switch
      (kbd "M-RET") 'bufler-list-buffer-peak
      (kbd "D")     'bufler-list-buffer-kill)

    (setq bufler-groups
          (bufler-defgroups
           ;; Subgroup collecting all named workspaces
           (group (auto-workspace))
           ;; Supgroup collecting buffers in a projectile project
           (group (auto-projectile))
           ;; Supgroup collecting all `help-mode' and `info-mode' buffers.
           (group
            (group-or "Help/Info"
                      (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                      (mode-match "*Info*" (rx bos "info-"))))
           ;; Supgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; to other groups so they end up grouped with their project buffers).
           (group
            (group-and "*Special*"
                       (name-match "**Special**"
                                   (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                       (lambda (buffer)
                         (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                              buffer)
                                     (funcall (mode-match "Dired" (rx bos "dired"))
                                              buffer)
                                     (funcall (auto-file) buffer))
                           "*Special*"))))
           ;; Group Remaining buffers my major-mode
           (auto-mode))))

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun rcool/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun rcool/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun rcool/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (rcool/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face '(:foreground "#62aeed"))
     (propertize " ॐ " 'face '(:foreground "white"))
     (propertize (rcool/get-prompt-path) 'face '(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face '(:foreground "white"))
        (propertize (concat " " current-branch) 'face '(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face '(:foreground "white"))
        (propertize package-version 'face '(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

(defun rcool/configure-eshell ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)
  (use-package xterm-color)
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserver-properties t)))
  (add-hook 'eshell-pre-command-hook
            '(lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            '(lambda () (setenv "TERM" "dumb")))

  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-beginning-of-input)
  (evil-normalize-keymaps)

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-prompt-regexp "^λ "
        eshell-highlight-prompt t
        eshell-hist-ignoredups t
        eshell-prompt-function 'rcool/eshell-prompt))
(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . rcool/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "nvim")))
  (eshell-git-prompt-use-theme 'powerline))

(rcool/leader-keys
  "oE" '(eshell :wk "Eshell (Non-Toggle)"))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-use-company-map t)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(use-package eshell-toggle
  :general
  (rcool/leader-keys
  "oe" '(eshell-toggle :wk "Eshell"))
  :custom
  (eshell-toggle-size-function 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package ivy-pass
    :commands ivy-pass
    :config
    (setq password-store-password-length 12)
    (setq epa-file-cache-passphrase-for-symmetric-encryption nil))

  (use-package auth-source-pass
    :config
    (auth-source-pass-enable))

  (rcool/leader-keys
    "P" '(:ignore t :wk "Passwords")
    "Pp" '(ivy-pass :wk "Ivy Pass")
  "Pi" '(password-store-insert :wk "Insert Password")
  "Pg" '(password-store-generate :wk "Generate Password")
    )

(use-package counsel-spotify
  :after ivy
  :config
  (setq counsel-spotify-client-id (password-store-get "API/Spotify/rcool-emacs-id"))
  (setq counsel-spotify-client-secret (password-store-get "API/Spotify/rcool-emacs-secret")))

(rcool/leader-keys
  "S" '(:ignore t :wk "Counsel Spotify")
  "Ss" '(:ignore t :wk "Search")
  "Ssp" '(counsel-spotify-search-playlist :wk "Search Playlist")
  "Sst" '(counsel-spotify-search-track :wk "Search Track")
  "Sp" '(counsel-spotify-toggle-play-pause :wk "Toggle Play Pause")
  "Sa" '(counsel-spotify-search-album :wk "Search Album")
  "S>" '(counsel-spotify-next :wk "Next")
  "S<" '(counsel-spotify-previous :wk "Previous")
  )

(use-package ivy-youtube
  :config
  (setq ivy-youtube-key (password-store-get "API/Youtube/rcool-emacs-api-key")))

(rcool/leader-keys
  "y" '(ivy-youtube :wk "Ivy Youtube"))

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
    :commands vterm
    :config
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
    (setq vterm-max-scrollback 10000)
)
  (use-package vterm-toggle
    :after vterm
    :init
    (setq vterm-toggle-fullscreen-p nil)
    (add-to-list 'display-buffer-alist
                 '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side . bottom)
                   (reusable-frames . visible)
                   (window-height . 0.3)))
    :general
    (rcool/leader-keys
      "ot" '(vterm-toggle-cd :wk "Vterm")))

(use-package ivy-pass
  :commands ivy-pass
  :config
  (setq password-store-password-length 12)
  (setq epa-file-cache-passphrase-for-symmetric-encryption nil))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(rcool/leader-keys
  "P" '(:ignore t :wk "Passwords")
  "Pp" '(ivy-pass :wk "Ivy Pass")
"Pi" '(password-store-insert :wk "Insert Password")
"Pg" '(password-store-generate :wk "Generate Password")
  )

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

(defun rcool/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
  )

(defun rcool/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
  )

(defun rcool/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-mode)
    (rcool/leave-focus-mode)
    (rcool/enter-focus-mode)))

(rcool/leader-keys
  "tf" '(rcool/toggle-focus-mode :wk "Focus Mode")
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter-fringe
  :diminish
  :config
  (setq git-gutter:update-interval 2)
  (setq-default left-fringe-width 5)
  (set-face-foreground 'git-gutter-fr:added "LightGreen")
  (fringe-helper-define 'git-gutter-fr:added nil
    ".XXXXXX."
    "XX....XX"
    "X......X"
    "X......X"
    "XXXXXXXX"
    "XXXXXXXX"
    "X......X"
    "X......X"
    )
  (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "XXXXXXXX"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X")
  (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "XXXXXX.."
    "XX....X."
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX....X."
    "XXXXXX..")
  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral")
  )

(use-package lorem-ipsum
  :ensure t
  :config
  (lorem-ipsum-use-default-bindings))

(use-package evil-nerd-commenter
  :general
  (rcool/leader-keys
    "/" '(evilnc-comment-or-uncomment-lines :wk "Comment Lines")))
;; INFO this is a test of the commenter, but not sure we are in prog mode
(use-package comment-tags
  :init
  (autoload 'comment-tags-mode "comment-tags-mode")
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
          ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  (setq comment-tags-comment-start-only t
        comment-tags-require-color nil 
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil)
  :config
  (add-hook 'prog-mode-hook 'comment-tags-mode))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(defun rcool/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadchrump-mode)
  (lsp))

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . rcool/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c s-p")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-io-doc-position 'bottom))

(use-package lsp-ivy)

(use-package dap-mode)

(use-package apheleia
  :config
  ;; Setup Prettier
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma" "es5"
              "--bracket-spacing" "true"
              "--single-quote" "true"
              "--semi" "true"
              "--print-width" "80"
              file))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js2-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(css-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(scss-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(less-css-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(json-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(graphql-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(yaml-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(vue-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(nxml-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(html-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(php-mode . prettier))




  (apheleia-global-mode t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(defun rcool/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq js-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook #'rcool/set-js-indentation)
  (add-hook 'json-mode-hook #'rcool/set-js-indentation)
  (require 'dap-node)
  (dap-node-setup))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|js[x]?\\|edge\\)\\'"
  :hook (web-mode . lsp-deferred)
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package impatient-mode
  :ensure t)

(use-package skewer-mode
  :ensure t)

(use-package css-mode
  :mode "\\.css\\'"
  :hook (css-mode . lsp-deferred)
  :config
  (setq css-indent-offset 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3"))

(use-package yaml-mode
  :hook (yaml-mode . lsp-deferred)
  :mode "\\.ya?ml\\'")
