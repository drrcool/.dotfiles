;; -*- lexical-binding: t; -*-

(setq
        mac-command-modifier 'super
        mac-right-command-modifier 'control
        mac-option-modifier 'meta
        mac-right-option-modifier 'meta
        mac-control-modifier       'meta
        mac-right-control-modifier 'control
)

(use-package! company
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idl-delay 0.0))

(setq user-full-name "Richard Cool"
      user-mail-address "richardjcool@gmail.com")

(setq-default delete-by-moving-to-trash t
              trash-directory "~/.local/share/Trash/files/")

(setq-default window-combination-resize t)

;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (counsel-ibuffer))

(after! undo-fu
  (setq undo-limit 10000000 ;; 1MB
        undo-strong-limit 100000000 ;;100MB
        undo-outer-limit 1000000000) ;; 1GB
(setq undo-fu-allow-undo-in-region t
      undo-fu-ignore-keyboard-quit t))
;;Evil undo
(after! evil
  (setq evil-want-fine-undo t))

(when (daemonp)
  (add-hook! '(delete-frame-functions delete-terminal-functions)
             (let ((inhibit-message t))
               (recentf-save-list)
               (savehist-save))))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package! modus-themes
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
        modus-themes-org-blocks 'gray-background
        modus-themes-markup '(intense background)
        modus-themes-mail-citations 'intensep
        modus-themes-lang-checkers '(background)
        modus-themes-completions
        '((matches . (extrabold intense accented))
          (selection . (semibold accented intense))
          (popup . (accented)))
        modus-themes-headings '((1 . (rainbow 1.4))
                                (2 . (rainbow 1.3))
                                (3 . (rainbow 1.2))
                                (4 . (rainbow bold 1.1))
                                (t . (rainbow bold)))
        modus-themes-org-blocks 'gray-background
        modus-themes-org-agenda
        '((header-block . (semibold 1.4))
          (header-date . (workaholic bold-today 1.2))
          (event . (accented italic varied))
          (scheduled . rainbow)
          (habit . traffic-light))
        modus-themes-markup '(intense background)
        modus-themes-mail-citations 'intense
        modus-themes-lang-checkers '(background))

  ;; (defun +modus-themes-tweak-packages ()
  ;;   (modus-themes-with-colors
  ;;     (set-face-attribute 'cursor nil :background (modus-themes-color 'blue))
  ;;     (set-face-attribute 'font-lock-type-face nil :foreground (modus-themes-color 'magenta-alt))
  ;;     (custom-set-faces
  ;;      ;; Tweak `evil-mc-mode'
  ;;      `(evil-mc-cursor-default-face ((,class :background ,magenta-intense-bg)))
  ;;      ;; Tweak `git-gutter-mode'
  ;;      `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
  ;;      `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
  ;;      `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg)))
  ;;      ;; Tweak `doom-modeline'
  ;;      `(doom-modeline-evil-normal-state ((,class :foreground ,green-alt-other)))
  ;;      `(doom-modeline-evil-insert-state ((,class :foreground ,red-alt-other)))
  ;;      `(doom-modeline-evil-visual-state ((,class :foreground ,magenta-alt)))
  ;;      `(doom-modeline-evil-operator-state ((,class :foreground ,blue-alt)))
  ;;      `(doom-modeline-evil-motion-state ((,class :foreground ,blue-alt-other)))
  ;;      `(doom-modeline-evil-replace-state ((,class :foreground ,yellow-alt)))
  ;;      ;; Tweak `diff-hl-mode'
  ;;      `(diff-hl-insert ((,class :foreground ,green-fringe-bg)))
  ;;      `(diff-hl-delete ((,class :foreground ,red-fringe-bg)))
  ;;      `(diff-hl-change ((,class :foreground ,yellow-fringe-bg)))
  ;;      ;; Tweak `solaire-mode'
  ;;      `(solaire-default-face ((,class :inherit default :background ,bg-alt :foreground ,fg-dim)))
  ;;      `(solaire-line-number-face ((,class :inherit solaire-default-face :foreground ,fg-unfocused)))
  ;;      `(solaire-hl-line-face ((,class :background ,bg-active)))
  ;;      `(solaire-org-hide-face ((,class :background ,bg-alt :foreground ,bg-alt)))
  ;;      ;; Tweak `display-fill-column-indicator-mode'
  ;;      `(fill-column-indicator ((,class :height 0.3 :background ,bg-inactive :foreground ,bg-inactive)))
  ;;      ;; Tweak `mmm-mode'
  ;;      `(mmm-cleanup-submode-face ((,class :background ,yellow-refine-bg)))
  ;;      `(mmm-code-submode-face ((,class :background ,bg-active)))
  ;;      `(mmm-comment-submode-face ((,class :background ,blue-refine-bg)))
  ;;      `(mmm-declaration-submode-face ((,class :background ,cyan-refine-bg)))
  ;;      `(mmm-default-submode-face ((,class :background ,bg-alt)))
  ;;      `(mmm-init-submode-face ((,class :background ,magenta-refine-bg)))
  ;;      `(mmm-output-submode-face ((,class :background ,red-refine-bg)))
  ;;      `(mmm-special-submode-face ((,class :background ,green-refine-bg))))))

  ;; (add-hook 'modus-themes-after-load-theme-hook #'+modus-themes-tweak-packages)

  :config
  (map! :leader
        :prefix "t" ;; toggle
        :desc "Toggle Modus theme" "m" #'modus-themes-toggle))

(setq doom-theme 'doom-vibrant)
(remove-hook 'Window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"
))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encodingto be LF UTF-8 so only show when its not"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                           t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))
    (map! :desc "Insert copilot suggestion" :i "C-t" #'copilot-accept-completion)

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Disable for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq tramp-default-method "ssh")

(use-package! org-auto-mode
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! org
(setq org-directory "~/Dropbox/orgmode/"
      org-log-done 'time
      org-list-allow-alphabetical t
      org-export-in-background nil
      org-export-async-debug t
      org-tags-column 1
      org-catch-invisible-edits 'smart
      org-export-with-sub-superscripts '{}
      org-pretty-entities-include-sub-superscripts nil
      org-auto-align-tags t
      org-special-ctrl-a/e t
      org-startup-indented t
      org-pretty-entities t
      org-startup-with-inline-images t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-image-actual-width '(300)
      org-insert-heading-respect-content t)

(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>" #'org-backward-heading-same-level
      :n "g <down>" #'org-borward-heading-same-level
      :n "g <left>" #'org-up-element
      :n "g <right>" #'org-down-element)

(setq org-tag-persistent-alist
      '(
        ("qoedash" . ?q)
        ("sessionwiz" . ?s)
        ("deviceReach" . ?d)
        ("adhoc" . ?a)
        ("chores" . ?c)
        ("urgent" . ?u)
        ("side-project". ?p)
        ("self-care". ?r)
        ("home". ?h)
        ("work". ?w)
        ("presentation". ?P)
        )
      )
(setq org-tag-faces
      '(("home"     . ( :foreground "white"))
        ("urgent" . (:weight bold))
        ("qoedash"  . (:background "#a43261"))
        ("sessionwiz" . (:background "#006ca5"))
        ("deviceReach" . (:background "#007086"))
        ("chores" . (:background "#6751a6"))
        ("adhoc" . (:background "#913e88"))
        ("side-project" . (:background "#0061b1"))
        ("self-care" . (:background "#ff9fc9"))
        ("work" . (:background "#3bd6ff"))
        ("presentation" . (:background "#d5b8ff"))
)
      )

;; Setup a custom Agenda view
(setq org-agenda-custom-commands
  '(
    ("q" "QoeDash" tags-todo "qoedash")
    ("s" "Sessionwiz" tags-todo "sessionwiz")
    ("h" "Chores" tags-todo "chore")
    ("d" "DeviceReachDash" tags-todo "chore")
    ("A" "adhoc" tags-todo "adhoc")
  ("c" "Custom Agenda"


  '((agenda "Schedule"
    ((org-agenda-span 'day)
    (org-deadline-warning-days 365)))
  (todo "TODO"
    ((org-agenda-overriding-header "Unscheduled Tasks")
      (org-agenda-files '("~/org/inbox.org" "~/org/work_notes/daily_notes.org"))
        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
 ))
  (todo "TODO"
    ((org-agenda-overriding-header "Unscheduled Project Tasks")
(org-agenda-files '("~/org/projects.org"))
        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
    ))))))
(setq org-agenda-files
      (list (expand-file-name "inbox.org" org-directory)
            (expand-file-name "agenda.org" org-directory)
            (expand-file-name "gcal-agenda.org" org-directory)
            (expand-file-name "notes.org" org-directory)
            (expand-file-name "projects.org" org-directory)
            (expand-file-name "archive.org" org-directory)))
(setq org-agenda-block-separator ?-
      org-agenda-time-grid '((daily today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             "--------" "------------------")
      org-agenda-current-time-string
      "<-- now ─────────────────────────────────────────────────")

(setq +org-capture-emails-file (expand-file-name "inbox.org" org-directory)
      +org-capture-todo-file (expand-file-name "inbox.org" org-directory)
      +org-capture-projects-file (expand-file-name "projects.org" org-directory))

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

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sublevel prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)

(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

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

(use-package! org-wild-notifier
  :hook (org-load . org-wild-notifier-mode)
  :config
  (setq org-wild-notifier-alert-time '(60 30)))

(use-package! org-menu
  :commands (org-menu)
  :init
  (map! :localleader
        :map org-mode-map
        :desc "Org menu" "M" #'org-menu))

(custom-set-faces!
  '(org-document-title :height 1.2))

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.000 . org-warning)
        (0.500 . org-upcoming-deadline)
        (0.000 . org-upcoming-distant-deadline)))

(setq org-fontify-quote-and-verse-blocks t)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(setq org-inline-src-prettify-results '("⟨" . "⟩")
      doom-themes-org-fontify-special-tags nil)

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶")
        org-modern-table-vertical 2
        org-modern-table-horizontal 4
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-footnote (cons nil (cadr org-script-display))
        org-modern-priority t
        org-modern-block t
        org-modern-block-fringe nil
        org-modern-horizontal-rule t
        org-modern-keyword
        '((t                     . t)
          ("title"               . "𝙏")
          ("subtitle"            . "𝙩")
          ("author"              . "𝘼")
          ("email"               . "@")
          ("date"                . "𝘿")
          ("lastmod"             . "✎")
          ("property"            . "☸")
          ("options"             . "⌥")
          ("startup"             . "⏻")
          ("macro"               . "𝓜")
          ("bind"                . #("" 0 1 (display (raise -0.1))))
          ("bibliography"        . "")
          ("print_bibliography"  . #("" 0 1 (display (raise -0.1))))
          ("cite_export"         . "⮭")
          ("print_glossary"      . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources"    . #("" 0 1 (display (raise -0.14))))
          ("export_file_name"    . "⇒")
          ("include"             . "⇤")
          ("setupfile"           . "⇐")
          ("html_head"           . "🅷")
          ("html"                . "🅗")
          ("latex_class"         . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header"        . "🅻")
          ("latex_header_extra"  . "🅻⁺")
          ("latex"               . "🅛")
          ("beamer_theme"        . "🄱")
          ("beamer_color_theme"  . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme"   . "🄱𝐀")
          ("beamer_header"       . "🅱")
          ("beamer"              . "🅑")
          ("attr_latex"          . "🄛")
          ("attr_html"           . "🄗")
          ("attr_org"            . "⒪")
          ("name"                . "⁍")
          ("header"              . "›")
          ("caption"             . "☰")
          ("RESULTS"             . "🠶")
          ("language"            . "𝙇")
          ("hugo_base_dir"       . "𝐇")
          ("latex_compiler"      . "⟾")
          ("results"             . "🠶")
          ("filetags"            . "#")
          ("created"             . "⏱")
          ("export_select_tags"  . "✔")
          ("export_exclude_tags" . "❌")))

  ;; Change faces
  (custom-set-faces! '(org-modern-tag :inherit (region org-modern-label)))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(when (modulep! :ui ligatures)
  (defadvice! +org-init-appearance-h--no-ligatures-a ()
    :after #'+org-init-appearance-h
    (set-ligatures! 'org-mode
                    :name nil
                    :src_block nil
                    :src_block_end nil
                    :quote nil
                    :quote_end nil)))

(use-package! org-ol-tree
  :commands org-ol-tree
  :config
  (setq org-ol-tree-ui-icon-set
        (if (and (display-graphic-p)
                 (fboundp 'all-the-icons-material))
            'all-the-icons
          'unicode))
  (org-ol-tree-ui--update-icon-set))

(map! :localleader
      :map org-mode-map
      :desc "Outline" "O" #'org-ol-tree)

(setq org-list-demote-modify-bullet
      '(("+"  . "-")
        ("-"  . "+")
        ("*"  . "+")
        ("1." . "a.")))
;; Org styling, hide markup etc.
(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis " ↩"
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
         (?C . 'all-the-icons-yellow)
         (?D . 'all-the-icons-green)
         (?E . 'all-the-icons-blue)))
);; closing paren for after org

(use-package! org-appear
  :hook (org-mode . org-appear-mode))

(use-package! org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda() (org-superstar-mode +1))))
(setq-default line-spacing 0)

(use-package! olivetti
  :init
  (setq olivetti-body-width 0.67)
  :config
  (defun distraction-free ()
    "Distraction free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))

(map!
 :leader
 :prefix "m"
 :map 'org-src-mode-map
 (:desc "Accept SRC Edits"
 :nmv "J" #'org-edit-src-exit)
 (:desc "Abort SRC Edits"
 :nmv "X" #'org-edit-src-abort))

(setq org-hugo-base-dir "~/org/markdown")

(use-package! beacon
  :config
  (beacon-mode +1))

;; Turn on line highlithting for current line
(hl-line-mode 1)
;; Add some margins
(set-fringe-mode 10)

(after! doom-modeline
    (setq
     doom-modeline-hud nil
     doom-modeline-minor-modes nil
          doom-modeline-height 15))

(setq doom-font (font-spec :family "Spleen32x64 Nerd Font" :size 20 :weight 'light))
(setq doom-variable-pitch-font (font-spec :family "Spleen32x64 Nerd Font" :size 16))

(plist-put! +ligatures-extra-symbols
        :and nil
            :or nil
            :for nil
            :not nil
            :true nil
            :false nil
            :int nil
            :float nil
            :str nil
            :bool nil
            :list nil


            )

(use-package! quickrun
   :defer t
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
    "cqs" '(quickrun-select :which-key "Select backend")
    "cq"  '(nil :which-key "quickrun")
    "cqq" '(quit-window :which-key "Quit")
    "cqr" '(quickrun :which-key "Run")
    "cqR" '(quickrun-region :which-key "Run Region")
    "cqa" '(quickrun-with-arg :which-key "Run with [A]rgs")
    "cqm" '(quickrun-autorun-mode :which-key "Toggle autorun mode")
    "cqs" '(quickrun-select :which-key "Select backend")
))

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(after! tree-sitter
(add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx))
)

(use-package! lsp-ui
  :after lsp
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . lsp-ui-sideline-mode))
  :config
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-position 'top)
  )

(use-package! tsi
  :hook ((web-mode-hook . tsi-typescript-mode)
         (typescript-mode-hook . tsi-typescript-mode)
         (tsx-mode-hook . tsi-typescript-mode)
          (json-mode-hook . tsi-typescript-mode)
          (css-mode-hook . tsi-css-mode)
          (scss-mode-hook . tsi-scss-mode))
  :config
  (require 'tsi-css)
  (require 'tsi-json)
  (require 'tsi-typescript)

)

;; (use-package! tsx-mode

;;   :hook (tsx-mode . lsp-deferred)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . tsx-mode))

;; (map!
;;  :leader
;;  :prefix "m"
;;  :map 'general-override-mode-map
;;  (:desc "Toggle All Nodes"
;;   :nmv #'tsx-mode-fold-toggle-all-nodes)
;;  (:desc "Toggle Coverage"
;;         :nmv #'tsx-mode-coverage-toggle)
;;  (:desc "Toggle Node"
;;         :nvm #'tsx-mode-fold-toggle-node)
;; )
;; )

(use-package! apheleia
  :config
  (apheleia-global-mode +1))

(general-def
  :prefix-map 'rc/lsp-map
"d"   #'lsp-find-declaration
"D"   #'lsp-ui-peek-find-definitions
"R"   #'lsp-ui-peek-find-references
"i"   #'lsp-ui-peek-find-implementation
"t"   #'lsp-find-type-definition
"s"   #'lsp-signature-help
"o"   #'lsp-describe-thing-at-point
"r"   #'lsp-rename

"f"   #'lsp-format-buffer
"m"   #'lsp-ui-imenu
"x"   #'lsp-execute-code-action
)
(hercules-def
:toggle-funs #'rc/lsp-map-mode
:keymap 'rc/lsp-map
:transient t)
(map!
 :leader
 :prefix "H"
 :desc "LSP"
 :nm "L" #'lsp-map-mode
 )

(general-def
:prefix-map 'rc/flycheck-map
   "f" #'flycheck-error-list-set-filter
   "j" #'flycheck-next-error
   "k" #'flycheck-previous-error
)
(hercules-def
 :toggle-funs #'rc/flycheck-mode
 :keymap 'rc/flycheck-map
 :transient t)
(map!
 :leader
 :prefix "H"
 :desc "Flycheck"
 :nm "f" #'rc/flycheck-mode)

(after! avy
  (setq avy-keys '(?n ?'))
(general-def
  :prefix-map 'rc/avy-map
"c" #'avy-goto-char-timer
"C" #'avy-goto-char
"w" #'avy-goto-wordi-1
"W" #'avy-goto-word-0
"l" #'avy-goto-line
"L" #'avy-goto-end-of-line
"m" #'avy-move-line
"M" #'avy-move-region
"k" #'avy-kill-whole-line
"K" #'avy-kill-region
"y" #'avy-copy-line
"Y" #'avy-copy-region
))
(hercules-def
 :toggle-funs #'rc/avy-mode
 :keymap 'rc/avy-map
 :transient t
)
(map!
 :leader
 :prefix "H"
 :desc "Avy"
 :nm "a" #'rc/avy-mode)

(use-package! org-sticky-header
  :config
  ( org-sticky-header-mode +1))

(after! doom-modeline
  (display-time-mode 1)

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker)))

(after! doom-modeline
  (let ((battery-str (battery)))
    (unless (or (equal "Battery Status Not Available" battery-str)
                (string-match-p (regexp-quote "unknown") battery-str)
                (string-match-p (regexp-quote "N/A") battery-str))

      (display-battery-mode 1))))

(after! doom-modeline
  (setq
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-hook! '+doom-dashboard-mode-hook (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(after! which-key
(setq which-key-idle-delay 0.1
      which-key-secondary-delay 0.05)
;; use a minibuffer
(which-key-setup-side-window-bottom)
(setq which-key-side-window-max-width 0.33)
(setq which-key-side-window-max-height 0.15)
(map!
  :nmv "C-<next>"  #'which-key-show-next-page-cycle :desc "Which-Key Next Page"
  :nvm "C-<prior>" #'which-key-show-previous-page-cycle :desc "Which-key Prior Page"
  )
)

(setq which-key-allow-multiple-replacements t)

(after! which-key
  (pushnew! which-key-replacement-alist
            '((""       . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "🅔·\\1"))
            '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)")       . (nil . "Ⓔ·\\1"))))

(set-frame-parameter (selected-frame) 'alpha '(95 100))
(add-to-list 'default-frame-alist '(alpha 95 100))

(use-package! focus
  :commands focus-mode)

(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)

(after! evil
  (evil-select-search-module 'evile-search-module 'isearch)
(setq evil-search-modful 'isearch)
(setq evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

(use-package! aggressive-indent
  :commands (aggressive-indent-mode))

(setq yas-triggers-in-field t)

(after! treemacs
(setq doom-themes-treemacs-enable-variable-pitch nil
      doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)
(setq treemacs-show-hidden-files nil
      treemacs-hide-dot-git-directory t
      treemacs-width 30
))

(setq eros-eval-result-prefix "⟹ ")

(after! lsp-mode
  (setq lsp-lens-enable t
        lsp-sematic-tokens-enable t
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(after! magit
 (setq magit-diff-refine-hunk t))

(use-package! conventional-commit
  :hook
  (git-commit-mode . conventional-commit-setup))

(setq company-global-modes
      '(not erc-mode
            circe-mode
            message-mode
            help-mode
            gud-mode
            vterm-mode
            org-mode))

(after! company-box
  (defun +company-box--reload-icons-h ()
    (setq company-box-icons-all-the-icons
          (let ((all-the-icons-scale-factor 0.8))
            `((Unknown       . ,(all-the-icons-faicon   "code"                 :face 'all-the-icons-purple))
              (Text          . ,(all-the-icons-material "text_fields"          :face 'all-the-icons-green))
              (Method        . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-red))
              (Function      . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-blue))
              (Constructor   . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-blue-alt))
              (Field         . ,(all-the-icons-faicon   "tag"                  :face 'all-the-icons-red))
              (Variable      . ,(all-the-icons-material "adjust"               :face 'all-the-icons-blue))
              (Class         . ,(all-the-icons-material "class"                :face 'all-the-icons-red))
              (Interface     . ,(all-the-icons-material "tune"                 :face 'all-the-icons-red))
              (Module        . ,(all-the-icons-faicon   "cubes"                :face 'all-the-icons-red))
              (Property      . ,(all-the-icons-faicon   "wrench"               :face 'all-the-icons-red))
              (Unit          . ,(all-the-icons-material "straighten"           :face 'all-the-icons-red))
              (Value         . ,(all-the-icons-material "filter_1"             :face 'all-the-icons-red))
              (Enum          . ,(all-the-icons-material "plus_one"             :face 'all-the-icons-red))
              (Keyword       . ,(all-the-icons-material "filter_center_focus"  :face 'all-the-icons-red-alt))
              (Snippet       . ,(all-the-icons-faicon   "expand"               :face 'all-the-icons-red))
              (Color         . ,(all-the-icons-material "colorize"             :face 'all-the-icons-red))
              (File          . ,(all-the-icons-material "insert_drive_file"    :face 'all-the-icons-red))
              (Reference     . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
              (Folder        . ,(all-the-icons-material "folder"               :face 'all-the-icons-red-alt))
              (EnumMember    . ,(all-the-icons-material "people"               :face 'all-the-icons-red))
              (Constant      . ,(all-the-icons-material "pause_circle_filled"  :face 'all-the-icons-red))

              (Struct        . ,(all-the-icons-material "list"                 :face 'all-the-icons-red))
              (Event         . ,(all-the-icons-material "event"                :face 'all-the-icons-red))
              (Operator      . ,(all-the-icons-material "control_point"        :face 'all-the-icons-red))
              (TypeParameter . ,(all-the-icons-material "class"                :face 'all-the-icons-red))
              (Template      . ,(all-the-icons-material "settings_ethernet"    :face 'all-the-icons-green))
              (ElispFunction . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-blue))
              (ElispVariable . ,(all-the-icons-material "adjust"               :face 'all-the-icons-blue))
              (ElispFeature  . ,(all-the-icons-material "stars"                :face 'all-the-icons-orange))
              (ElispFace     . ,(all-the-icons-material "format_paint"         :face 'all-the-icons-pink))))))

  (when (daemonp)
    ;; Replace Doom defined icons with mine
    (when (memq #'+company-box--load-all-the-icons server-after-make-frame-hook)
      (remove-hook 'server-after-make-frame-hook #'+company-box--load-all-the-icons))
    (add-hook 'server-after-make-frame-hook #'+company-box--reload-icons-h))

  ;; Reload icons even if not in Daemon mode
  (+company-box--reload-icons-h))

(setq ivy-posframe-display-functions-alist
      '((swiper                    . ivy-posframe-display-at-point)
        (complete-symbol           . ivy-posframe-display-at-point)
        (counsel-M-x               . ivy-posframe-display-at-point)
        (counsel-esh-history       . ivy-posframe-display-at-window-center)
        (counsel-describe-function . ivy-display-function-fallback)
        (counsel-describe-variable . ivy-display-function-fallback)
        (counsel-find-file         . ivy-posframe-display-at-point)
        (counsel-recentf           . ivy-posframe-display-at-point)
        (counsel-register          . ivy-posframe-display-at-point)
   (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

;; Macro which creates advice template
(defmacro rc/with-advice (adlist &rest body)

  "Execute BODY with advice in ADLIST.

Each element of ADLIST should be a list of the form
(SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to advice-add. The BODY is wrapped in an
unwind-protect form so the advice will be removed even in the event of an error
or  nonlocal exit."

  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
                 adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove
                         ,(car adform)
                         ,(nth 2 adform)))
                 adlist))))

(defun make-display-buffer-matcher-function (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

(defun mp-buffer-has-project-p (buffer action)
  (with-current-buffer buffer (project-current nil)))

(add-to-list 'display-buffer-alist
             `(,(rx (| "xref*"
                       "*grep*"
                       "*Occur*"))
               display-buffer-reuse-window
               (inhibit-same-window . nill)))

(setq magit-display-buffer-function #'display-buffer)

(add-to-list 'display-buffer-alist
             `(,(make-display-buffer-matcher-function '(magit-mode))
               (display-buffer-reuse-mode-window
                display-buffer-in-direction)
               (mode magit-mode)
               (window . root)
               (window-width . 0.15)
               (direction . left)))

(setq window-sides-slots '(0 0 1 1))
(add-to-list 'display-buffer-alist
             '("\\*e?shell\\*" display-buffer-in-direction
               (direction . bottom)
               (window . root)
               (window-height . 0.3)))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*compilation*" "*grep*" "\\*vterm\\*"))
               display-buffer-in-side-window
               (side . right)
               (slot . 0)
               (window-parameters . ((no-delete-other-windows . t)))
               (window-width . 0.2)))

(after! evil
(hercules-def
 :show-funs #'windresize
 :hide-funs '(windresize-exit windresize-cancel-and-quit)
 :keymap 'windresize-map)
(map!
:map doom-leader-toggle-map
:leader
:prefix "t"
:nm "S" #'window-toggle-side-windows :desc "Sidebar"
)

                (map!
 :map my-evil-window-map
 :leader
 :prefix ("w" . "window")

 :nm "v" #'+evil/window-vsplit-and-follow
 :nm "s" #'+evil/window-split-and-follow
 :nm "h" #'evil-window-left
 :nm "l" #'evil-window-right
 :nm "j" #'evil-window-down
 :nm "k" #'evil-window-up
 :nm "x" #'evil-window-exchange
 :nm "u" #'winner-undo
 :nm "d" #'ace-delete-window
 :nm "a" #'ace-window
 :nm "S" #'ace-swap-window
 :nm "m" #'maximize-window
 :nm "w" #'windresize)
)

(use-package! kaolin-themes

:config
(load-theme  'kaolin-dark t)
(kaolin-treemacs-theme)
)

;;; Code:
(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))
