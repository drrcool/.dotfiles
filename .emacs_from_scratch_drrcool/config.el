(setq user-full-name "Richard Cool"
      user-mail-address "richardjcool@gmail.com")

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
                         ("org" . "https://orgmode.org/elpa/")
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

(setq inhibit-startup-messages t)
(scroll-bar-mode -1) ;; Disable Visible Scrollbars
(tool-bar-mode -1)   ;; Disable the toolbar
;;(tooltip-mode -1)  ;; Disable tooltip
(set-fringe-mode 10) ;; Give some breathing room
(menu-bar-mode -1)   ;; Disable the meu bar

(setq initial-scratch-message "; Hello Doctor Cool. C-x C-f eh" ) ;; Message on Scratch Buffer

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rcool/leader-keys
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer rcool/local-leader-keys
    :prefix ","
    :global-prefix "SPC m"))

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

(defun rcool/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode 1))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
  (setq org-agenda-files
        '("~/org/birthdays.org"
          "~/org/inbox.org"
          "~/org/journal.org"
          "~/org/notes.org"
          "~/org/projects.org"
          "~/org/notes.org"
          "~/org/work.org"))
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


  )

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
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  :general
  (rcool/leader-keys
    :states '(normal visual motion)
    :prefix "SPC"
   "" nil
   "d" '(:ignore t :which-key "+Daily Notes")
   "d t" '(org-roam-dailies-goto-today :wk "Today's Daily Note")
   "d y" '(org-roam-dailies-goto-yesterday :wk "Yesterday's Daily Note")
   )
  (rcool/local-leader-keys
    :states '(normal visual motion)
   :keymaps 'org-mode-map
   "r" '(:ignore t :which-key "+Roam")
   "b" '(:ignore t :wk "+Babel")
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
         "#+filetags: Institution CRM\n\n* Contracts\n\nRelationship: %^{Relationship}\nPhone:\nAddress\n\n* Notes\\n %?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
         :unnarrowed t)
        ))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs_from_scratch_drrcool/snippets"))
  )
