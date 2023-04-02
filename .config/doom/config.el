(use-package! doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-Iosvkem t)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defvar rcool/black-color "#1F2528")
(defvar rcool/red-color "#EC5F67")
(defvar rcool/yellow-color "#FAC863")
(defvar rcool/blue-color "#6699CC")
(defvar rcool/green-color "#99C794")
(defvar rcool/purple-color "#C594C5")
(defvar rcool/teal-color "#5FB3B3")
(defvar rcool/light-grey-color "#C0C5CE")
(defvar rcool/dark-grey-color "#65737E")

(setq mac-command-modifier 'super
      mac-right-command-modifier 'control
      mac-option-modifier 'meta
      mac-right-option-modifier 'meta
      mac-control-modifier 'meta
      mac-right-control-modifier 'control)

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion  )
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilor-accept-completion-by-word)))

(use-package doct
  :commands (doct))

(after! org-capture
  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
(let ((name (pop declaration))
    (set (intern (concat "all-the-icons-" (plist-get declaration :set))))
    (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
    (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
  (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates
            (mapcar (lambda (template)
                      (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                  (spec (plist-get (plist-get props :doct) :icon)))
                        (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                       "\t"
                                                       (nth 1 template))))
                      template)
                    templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))
  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? ✉️"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web")
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch")
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info")
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea")))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra "")

                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t")

                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t")))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file)))
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file)))))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))

(after! org
  (setq org-directory "~/org/"
        org-log-done 'time
        org-tags-column 1
        org-auto-align-tags t
        org-pretty-entities t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'reorganize-frame)
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
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
          ("@recovery" . ,rcool/blue-color)))

)

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
          '(("d" "default" plain
           ""
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}\n\n")
           :unnarrowed t)
          ("a" "area" plain
           "#+filetags: Area\n\n* Goals\n\n%^{Goals}\n\n* Tasks\n\n** TODO %?"
           :if-new (file+head "%<%<%Y%m%d%H%M%S>-${slug}.org" "${title}")
           :unnarrowed t)
          ("j" "project" plain
           "#+filetags: Project\n\n* Goals\n\n%^{{Goals}\n\n* Tasks\n\n TODO %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}")
           :unnarrowed t)
          ("p" "people" plain
           "#+filetags: People CRM\n\n* Contacts\n\nRelationship: %^{Relationship}\nPhone:\nAddress\nBirthday\n\n* Notes\n\n %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}")
           :unnarrowed t)
          ("i" "institution" plain
           "#+filetags: Institution CRM\n\n* Contracts\n\nRelationship: %^{Relationship}\nPhone:\nAddress\n\n* Notes\n\n %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}")
           :unnarrowed t)
          ))
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-dailies-directory "daily/")
  (org-roam-complete-everywhere t)

  )

(defun rcool/org-journal-find-location()
  "Open today's journal, but specify a non-nill prefix arguement in order to
   inhibit inserting the heading;"
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
              (org-narrow-to-subtree))
    (goto-char (point-max)))

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
(map! :leader
      :desc "Refresh Agenda Files" "n A" #'rcool/define-agenda-files )
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

(use-package! org-super-agenda
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

(setf (alist-get 'height +org-capture-frame-parameters) 15)
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   (org . t)
   (sqlite . t)
   (js . t)
   (lisp . t)
   (css . t)
))
(setq python-shell-completion-native-enable nil)
(setq org-src-window-setup 'current-window)
(defun org-babel-execute:typescript (body params)
                                          (let ((org-babel-js-cmd "npx ts-node < "))
                                            (org-babel-execute:js body params)))

(use-package! org-menu
  :commands (org-menu)
  :init
  (map! :localleader
        :map org-mode-map
        :desc "Org Menu" "M" #'org-menu))

(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>" #'org-backward-heading-same-level
        :n "g <down>" #'org-forward-heading-same-level
        :n "g <left>" #'org-up-element
        :n "g <right>" #'org-down-element
        )

(defun rcool/presentation-setup ()
  (setq text-scale-mode-amount 3)
  (org-display-inline-images)
  (hide-mode-line-mode 1)
  (text-scale-mode 1))

(defun rcool/presentation-end ()
  (hide-mode-line-mode 0)
  (text-scale-mode 0))

(use-package! org-tree-slide
  :hook ((org-tree-slide-play . rcool/presentation-setup)
         (org-tree-slide-stop . rcool/presentation-end))
  :custom
  (org-tree-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation Started")
  (org-tree-slide-deactivate-message "Presentation Ended")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // ")
  (org-image-actual-width nil))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  (run-at-time nil nil #'org-appear--set-elements))

(setq org-list-demote-modify-bullet
          '(("+"  . "-")
        ("-"  . "+")
        ("*"  . "+")
        ("1." . "a.")))
(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis " ▾"
        org-hide-leading-stars t
        org-startup-indented t
        )

(setq doom-font (font-spec :family "Spleen32x64 Nerd Font" :size 20 :Weight 'light))
(setq doom-variable-pitch-font (font-spec :family "Spleen32x64 Nerd Font" :size 16))

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(use-package! jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode web-mode)
  )
(map! :leader
      (:prefix ("j" . "Jest")
       :desc "Running Tests in Buffer" "b" #'jest-test-run
       :desc "Run with Debuffer" "d" #'jest-test-debug
       :desc "Rerun last test" "r" #'jest-test-rerun-test
       :desc "Run test at point" "p" #'jest-test-run-at-point
       :desc "Run all tests in project" "a" #'jest-test-run-all-tests
       :desc "Rerun last with debugger" "R" #'jest-test-debug-rerun-test
       :desc "Run test at point with debugger" "P" #'jest-test-debug-run-at-point
       )
      )

(use-package! lsp-ui
  :after lsp
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . lsp-ui-sideline-mode))
  :config
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-position 'top
        lsp-lens-enable t
        lsp-semantic-tokens-enable t
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbols nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t)
  )

(use-package! tsi
  :hook ((web-mode-hook . tsi-typescript-mode)
         (typescript-mode-hook . tsi-typescript-mode)
         (tsx-mode-hook . tsi-typescript-mode)
         (json-mode-hook . tsi-typescript-mode)
         (css-mode-hook . tsi-typescript-mode)
         (scss-mode-hook . tsi-typescript-mode)
      )
  :config
  (require 'tsi-css)
  (require 'tsi-json)
  (require 'tsi-typescript)
)

(use-package! web-mode
  :hook (web-mode-hook . lsp)
                )
