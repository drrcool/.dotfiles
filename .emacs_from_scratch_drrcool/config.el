(setq user-full-name "Richard Cool"
      user-mail-address "richardjcool@gmail.com")

(defvar rcool/black-color "#1F2528")
(defvar rcool/red-color "#EC5F67")
(defvar rcool/yellow-color "#FAC863")
(defvar rcool/blue-color "#6699CC")
(defvar rcool/green-color "#99C794")
(defvar rcool/purple-color "#C594C5")
(defvar rcool/teal-color "#5FB3B3")
(defvar rcool/light-grey-color "#C0C5CE")
(defvar rcool/dark-grey-color "#65737E")

(setq gc-cons-threshold (* 50 1000 1000))
;; Profile Emacs Startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Tells use package to use straight.el to download
(setq straight-use-package-by-default t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;; Disable Visible Scrollbars
(tool-bar-mode -1)   ;; Disable the toolbar
;;(tooltip-mode -1)  ;; Disable tooltip
(set-fringe-mode 10) ;; Give some breathing room
(menu-bar-mode -1)   ;; Disable the meu bar

(setq initial-scratch-message "; Hello Doctor Cool. C-x C-f eh" ) ;; Message on Scratch Buffer

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(use-package dashboard
  :diminish
  (dashboard-mode page-break-lines-mode)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to emacs. ")
  (setq dashboard-items '((recents . 2)
                          (projects . 2)
                          (agenda . 10)))
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-week-agenda t)
  (setq dashboard-startup-banner 'logo)
  :custom-face
  (dashboard-heading ((t (:foreground "#fff" :weight bold))))
  )

(defvar rcool/default-font-size 200)
  (set-face-attribute 'default nil :font "Spleen32x64 Nerd Font" :height rcool/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "Spleen32x64 Nerd Font" :height 210)
  (set-face-attribute 'variable-pitch nil :font "Spleen32x64 Nerd Font" :height 220 :weight 'regular)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15))

(use-package hide-mode-line)

(use-package doom-themes :defer t)
(load-theme 'doom-acario-dark t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (use-package general
    :config
    (general-evil-setup t)
    (general-create-definer rcool/leader-keys
      :states '(normal visual motion emacs)
      :prefix "SPC"
      :global-prefix "C-SPC"
      )
    (general-create-definer rcool/local-leader-keys
      :states '(normal visual motion emacs)
      :prefix ","
      :global-prefix "SPC m"
      ))
  (rcool/leader-keys
    "" nil
    "f" '(:ignore t :wk "Files")
    "w" '(:ignore t :wk "Window")
    "b" '(:ignore t :wk "Buffer")
    "TAB" '(:ignore t :wk "Workspace")
    "c" '(:ignore t :wk "Code")
    "g" '(:ignore t :wk  "Git")
    "h" '(:ignore t :wk "Help")

    "i" '(:ignore t :wk "Insert")
    "n" '(:ignore t :wk "Notes")
    "o" '(:ignore t :wk "Open")
    "p" '(:ignore t :wk "Project")
    "q" '(:ignore t :wk "Quit")
   "t" '(:ignore t :wk "Toggle")
) 
    (rcool/local-leader-keys
      "" nil)

(defun rcool/disable-arrow-keys ()
  (interactive)
  (message "STOP USING ARROW KEYS!"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode-buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "<left>") 'rcool/disable-arrow-keys)
  (define-key evil-normal-state-map (kbd "<down>") 'rcool/disable-arrow-keys)
  (define-key evil-normal-state-map (kbd "<right>") 'rcool/disable-arrow-keys)
  (define-key evil-normal-state-map (kbd "<up>") 'rcool/disable-arrow-keys)
  (evil-global-set-key 'motion (kbd "<left>") 'rcool/disable-arrow-keys)
  (evil-global-set-key 'motion (kbd "<down>") 'rcool/disable-arrow-keys)
  (evil-global-set-key 'motion (kbd "<right>") 'rcool/disable-arrow-keys)
  (evil-global-set-key 'motion (kbd "<up>") 'rcool/disable-arrow-keys)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(setq
 mac-command-modifier 'super
 mac-right-command-modifier 'control
 mac-option-modifier 'meta
 mac-right-option-modifier 'meta
 mac-control-modifier 'meta
 mac-right-control-modifier 'control
 )

(use-package command-log-mode)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  :general
  (rcool/leader-keys
    :states '(normal visual motion)
    "s" '(:ignore t :wk "Search")
    "s s" '(swiper :wk "Swiper")
    ))

(use-package ivy-rich
  :after counsel
  :init
  (ivy-rich-mode 1))

(use-package counsel
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :general
    (rcool/leader-keys
      ":" '(counsel-M-x :wk "M-x")
"b i" '(counsel-ibuffer :wk "iBuffer")
"b b" '(counsel-switch-buffer :wk "Switch Buffer")
      ) 
   :config
    (counsel-mode 1))

  (use-package smex
    :defer 1
    :after counsel)

(use-package helpful
:custom
(counsel-describe-function-function #'helpful-callable)
(counsel-describe-variable-function #'helpful-variable)
:bind
([remap describe-function] . counsel-describe-function)
([remap describe-commpand] . helpful-command)
([remap describe-variable] . counsel-describe-variable)
([remap describe-key] . helpful-key))

(use-package autopair)
(autopair-global-mode)

(use-package evil-smartparens
  :hook (smartparens-enabled-hook . evil-smartparens-mode)
  (prog-mode . evil-smartparens-mode))

(use-package emmet-mode
  :mode "\\.edge\\'"
  :diminish (emmet-mode . "ε")
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq emmet-expand-jsx-className? nil)
  (setq emmet-self-closing-tag-style " /")
  )

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
          "Scale text"
          ("j" text-scale-increase "in")
          ("k" text-scale-decrease "out")
          ("f" nil "finished" :exit t))

(rcool/leader-keys
 :states 'normal
 "h" '(:ignore t :wk "Hydra")
 "h s" '(hydra-text-scale/body :wk "Scale Text"))

(defun rcool/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode 1)
(auto-fill-mode 0)
(setq evil-auto-indent nil))

(defun rcool/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Spleen32x64 Nerd Font" :weight 'regular :height (cdr face)))

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . rcool/org-mode-setup)
  :ensure org-plus-contrib
  :config
  (setq org-src-fontify-natively t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-edit-src-content-indentation 2)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-block-startup nil)
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (rcool/org-font-setup)

  :general
  (rcool/local-leader-keys
    :states '(normal visual motion)
    :keymaps 'org-mode-map
    "'" '(org-edit-special :wk "Edit Special")
    "-" '(org-babel-demarcate-block :wk "Split Block")
    "z" '(org-babel-hide-result-toggle :wk "Fold Result"))
  (rcool/local-leader-keys
    :keymaps 'org-scr-mode-map
   :states '(normal motion visual)
    "'" '(org-edit-src-exit :wk "exit"))
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (rcool/leader-keys
    :states '(normal visual motion)
    "a" '(org-agenda :wk "Agenda")
    "x" '(org-capture :wk "Capture")
  ))

(setq org-tag-persistent-alist
      '(("Inbox" . ?i)
        ("@home" . ?h)
        ("@work" . ?w)
        ("@recovery" . ?r)
        ("@Manny" . ?m)
        ("@car" . ?c)
        ("#phone" . ?p)
        ("#computer" . ?u)))

(setq org-tag-faces
      '(("@home" . ,rcool/green-color)
        ("@car" . ,rcool/purple-color)
        ("@work" . ,rcool/red-color)
        ("Inbox" . ,rcool/teal-color)
        ("@recovery" . ,rcool/blue-color)
        ))

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
         "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
        ("j" "Journal Entries")
        ("jj" "Journal" entry
         (file+olp+datetree "~/org/journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in
         :clock-resume
         :empty-lines 1)
        ("jm" "Meeting" entry
         (file+olp+datetree "~/org/journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in
         :clock-resume
         :empty-lines 1)
        ("w" "Workflow")
        ("we" "Checking Email" entry (file+olp+datetree "~/org/journal.org")
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)))

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
  :config

  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  :general
  (rcool/leader-keys
    :states '(normal visual motion)
    :prefix "SPC"
    "X" '(org-roam-capture :wk "Roam Capture")
    "n t" '(org-roam-dailies-goto-today :wk "Today's Daily Note")
    "n y" '(org-roam-dailies-goto-yesterday :wk "Yesterday's Daily Note")
    "a" '(rcool/define-agenda-files :wk "Refresh Agenda DB"))
  (rcool/local-leader-keys
    :states '(normal visual motion)
    :keymaps 'org-mode-map
    "r" '(:ignore t :which-key "Roam")
    "b" '(:ignore t :wk "Babel")
    "b t" '(org-babel-tangle :wk "Tangle")
    "i" '(completion-at-point :wk "Completion at Point")
    "r f" '(org-roam-node-find :wk "Find Node")
    "r i" '(org-roam-node-insert :wk "Insert Node")
    "r c" '(rcool/org-roam-create-id :wk "Create Roam ID")
    "r p" '(org-roam-dailies-goto-previous-note :wk "Prev Daily Note")
    "r n" '(org-roam-dailies-goto-next-note :wk "Next Daily Note")
    "r b" '(org-roam-buffer-toggle :wk "Toggle Buffer")
    ))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"p
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+TITLE: %<%Y-%m-%d>\n#+filetags: Daily\n\n"))))

(defvar rcool/org-created-property-name "CREATED")

(defun rcool/org-set-created-property (&optional active name)
  (interactive)
  (let* ((created (or name rcool/org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now)
      now)))

(defun rcool/org-find-time-file-property (property &optional anywhere)
  (save-execursion
   (goto-char (point-min))
   (let ((first-heading
          (save-excursion
            (re-search-forward org-outline-regexp-bol nil t))))
     (when (re-search-forward (format "^#\\+%s:" property)
                              (if anywhere nil first-heading) t)
       (point)))))

(defun rcool/org-has-time-file-property-p (property &optional anywhere)
  (when-let ((pos (rcool/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos -1))))


(defun rcool/org-set-time-file-property (property &optional anywhere pos)
  (when-let ((pos (or pos
                      (rcool/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun rcool/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (rcool/org-set-time-file-property "LAST_MODIFIED")))

(defun rcool/org-roam-create-id ()
 (interactive)
 (org-id-get-create)
 (rcool/org-set-created-property))

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' function.
Note the weekly scope of the command's precision.")

(defun insert-current-time ()
  "Insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert "* ")
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n")
  )

(rcool/leader-keys
  :states '(normal visual motion)
  :keymap 'org-mode-map
  "," '(insert-current-time :wk "current time"))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n")
         :unnarrowed t)
        ("a" "area" plain
         "#+filetags: Area\n\n* Goals\n\n%^{Goals}\n\n* Tasks\n\n** TODO %?"
         :if-new (file+head "%<%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
        ("j" "project" plain
         "#+filetags: Project\n\n* Goals\n\n%^{{Goals}\n\n* Tasks\n\n TODO %?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
        ("p" "people" plain
         "#+filetags: People CRM\n\n* Contacts\n\nRelationship: %^{Relationship}\nPhone:\nAddress\nBirthday\n\n* Notes\n\n %?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
        ("i" "institution" plain
         "#+filetags: Institution CRM\n\n* Contracts\n\nRelationship: %^{Relationship}\nPhone:\nAddress\n\n* Notes\n\n %?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
        ))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("•" "•" "•" "◦" "◦" "◦" "◦")))



(defun rcool/define-agenda-files ()
  (interactive)
  "Return a list of note files containing 'HasTodo' tag.  I use this to denote files with tasks for org-agenda"
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (in tag $v1)] '(["Project" "Area" "Daily"])))))

  ;; Roam daily and project files only
  (setq org-agenda-files (rcool/define-agenda-files))

(defun rcool/buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun rcool/agenda-category (&optional len)
  "Get category of item at point for agenda."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (rcool/buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(setq org-agenda-prefix-format
      '((agenda . " %i %(rcool/agenda-category 32)%?-32t% s")
        (todo . " %i %(rcool/agenda-category 32) ")
        (tags . " %i %(rcool/agenda-category 32) ")
        (search . " %i %(rcool/agenda-category 32) ")))

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-dim-blocked-tasks nil))

;;Dashboard View
(setq org-super-agenda-groups
      '((:name "Priority"
               :priority "A")
        (:name "Inbox"
               :tag ("Inbox" "Daily"))
        (:name "Next Actions for Work"
               :and (
                     :todo ("NEXT")
                           :tag ("Active")
                           :tag ("@work")))
        (:name "Next Actions at Home"
               :and (
                     :todo ("NEXT")
                           :tag ("Active")
                           :tag ("@home")))
        (:name "Waiting"
               :todo "WAIT")
        (:name "Home"
               :tag "@home")
        (:name "Work"
               :tag "@work")
        (:name "Productivity"
               :tag "Productivity")))
(org-super-agenda-mode)

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "TODO"
                ((org-agenda-overriding-header "TODO Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
        ("n" "TODO Tasks"
         ((todo "TODO"
                ((org-agenda-overriding-header "Todo Tasks")))))
        ("h" "Home Tasks" tags-todo "+@home")
        ("w" "Work Tasks" tags-todo "+@work")
        ("u" "Computer Tasks" tags-todo "+#computer")
        ("r" "Recovery Tasks" tags-todo "+@recovery")
        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Taskss")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))

        ("w" "Workflow Status"
         ((todo "WAIT"
                ((org-agenda-overriding-header "Waiting on External")
                 (org-agenda-files org-agenda-files)))
          (todo "REVIEW"
                ((org-agenda-overriding-header "In Review")
                 (org-agenda-files org-agenda-files)))
          (todo "PLAN"
                ((org-agenda-overriding-header "In Planning")
                 (org-agenda-files org-agenda-files)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Project Backlog")
                 (org-agenda-files org-agenda-files)))
          (todo "READY"
                ((org-agenda-overriding-header "Ready for Work")
                 (org-agenda-files org-agenda-files)))
          (todo "ACTIVE"
                ((org-agenda-overriding-header "Active Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "COMPLETED"
                ((org-agenda-overriding-header "Completed Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "CANC"
                ((org-agenda-overriding-header "Cancelled Projects")
                 (org-agenda-files org-agenda-files)))))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (sql . t)
   (calc . t)
   (lua . t)))
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package org-special-block-extras
  :ensure t
  :hook (org-mode . org-special-block-extras-mode))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("temp" . "src"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("html" . "src html"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("lua" . "src lua"))

(defun rcool/org-babel-tangle-config ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rcool/org-babel-tangle-config)))

(use-package org-alert
  :ensure t
  :custom (alert-default-style 'osx-notifier)
  :config
  (setq org-alert-interval 1809
        org-alert-notification-title "Reminde!")
  (org-alert-enable))

(use-package org-wild-notifier
  :ensure t
  :custom
  (alert-default-style 'osx-notifier)
  (org-wild-notifier-alert-time '(1 10 30))
  (org-wild-notifier-keyword-whitelist '("TODO"))
  (org-wild-notifier-notificiation-title "Org Wild Reminder!")
  :config
  (org-wild-notifier-mode 1))

(defun rcool/presentation-setup ()
  (setq text-scale-mode-amount 3)
  (org-display-inline-images)
  (hide-mode-line-mode 1)
  (text-scale-mode 1))

(defun rcool/presentation-end ()
  (hide-mode-line-mode 0)
  (text-scale-mode 0))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . rcool/presentation-setup)
         (org-tree-slide-stop . rcool/presentation-end))
  :custom
  (org-tree-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation Started")
  (org-tree-slide-deactivate-message "Presentation Ended")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // ")
  (org-image-actual-width nil))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (defun rcool/set-markdown-header-font-sizes ()
    (font-lock-add-keywords 'markdown-mode
                            '(("^*\\([-]\\)"
                               (0 (prog1 (compose-region (match-beginning 1) (match-end 1) "•"))))))
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :font "Hack" :weight 'normal :height (cdr face)))
    (set-face-attribute 'markdown-code-face nil :inherit '(shadow fixed-pitch)))

  (defun rcool/markdown-mode-hook ()
    (rcool/set-markdown-header-font-sizes))
  (add-hook 'markdown-mode-hook 'rcool/markdown-mode-hook))

(use-package ox-reveal
  :ensure t
  :config
  (require 'ox-reveal)
  (setq org-reveal-root "https://cdn.jsdelivr/net/npm/reveal.js")
  (setq org-reveal-mathjax t))

(use-package htmlize
  :ensure t)

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :commands emojify-mode)

(use-package counsel-osx-app
  :bind* ("S-M-SPC" . counsel-osx-app)
  :commands counsel-osx-app
  :config
  (setq counsel-osx-app-location
        (list "/Applications"
              "/Applications/Misc"
              "/Applications/Utilities"
              (expand-file-name "~/Applications")
              "/Applications/Xcode.app/Contents/Applications")))

(use-package speed-type
  :ensure t)

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

(use-package lsp-treemacs
  :init (treemacks-display-current-project-exclusively)
  :after lsp)
(rcool/leader-keys
  "e" '(treemacs :wk "Explorer")
  "t e" '(treemacs :wk "Explorer")
  "p e" '(treemacs-display-current-project-exclusively :wk "Project Explorer"))

(use-package dap-mode)

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

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package company
  :after lsp-mode
  :hook ((lsp-mode . company-mode)
         (eldoc-mode . company-mode))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

(use-package company-box
  :diminish
  :functions (all-the-icons-faicon
              all-the-icons-material
              all-the-icons-octicon
              all-the-icons-alltheicon)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-enable-icon (display-graphic-p))
  :config
  (setq company-box-backends-colors nil))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectiled-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/projects")
    (setq projectile-project-search-path '("~/Documents/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter-fringe
  :diminish
  :config
  (setq git-gutter:update-interval 2)
  (setq-default left-fringe-width 20)
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

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs_from_scratch_drrcool/snippets"))
  )

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

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

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

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
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
    "t-" '(dired :wk "Dired")))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "open")
                                ("jpg" . "open")
                                ("jpeg" . "open")
                                ("pdf" . "open")
                                ("mov" . "open")
                                ("html" . "open"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

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
