(use-package! doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
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
(defvar rcool/qoedash-color "#B533FF")
(defvar rcool/sessionwiz-color "#338AFF")

(setq mac-command-modifier 'super
      mac-right-command-modifier 'control
      mac-option-modifier 'meta
      mac-right-option-modifier 'meta
      mac-control-modifier 'meta
      mac-right-control-modifier 'control)

  (use-package! copilot
	:hook (prog-mode . copilot-mode)
  )

  (defun rcool/copilot-tab ()
	(interactive)
	(or (copilot-accept-completion)
		(indent-for-tab-command)))

  (with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
  (kbd "<tab>") #'rcool/copilot-tab))

(use-package! dired-rainbow
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
          ("#qoedash" . ?Q)
          ("#sessionwiz" . ?S)
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
          ("#qoedash" . ,rcool/qoedash-color)
          ("#sessionwiz" . ,rcool/sessionwiz-color)
          ("@recovery" . ,rcool/blue-color)))

)

  (use-package org-roam

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

;; (setq doom-font (font-spec :family "PragmataProMonoLiga Nerd Font" :size 23 :weight 'light))
;; (setq doom-font (font-spec :family "Spleen32x64 Nerd Font" :size 20 :weight 'light))
 ;; (setq doom-font (font-spec :family "OperatorMonoLig Nerd Font" :size 17 :Weight 'light))
;; (setq doom-font (font-spec :family "DankMono Nerd Font" :size 23 :weight 'light))
(setq doom-font (font-spec :family "VictorMono Nerd Font Mono" :size 23 :weight 'light))
;; (setq doom-font (font-spec :family "Fira Code" :size 16 :Weight 'light))
(setq doom-variable-pitch-font (font-spec :family "JuliaMono" :size 14))

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

(defun rcool/org-babel-tangle-config ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))
(add-hook 'org-mode-hook '(lambda() (add-hook 'after-save-hook #'rcool/org-babel-tangle-config)))

(use-package!
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

  (setq display-time-default-load-average nil)
  (line-number-mode)
  (column-number-mode)
  (display-time-mode)
  (size-indication-mode 0)

  (use-package! hide-mode-line
	:commands (hide-mode-line-mode))

  (use-package! doom-modeline
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

  (use-package! org-superstar
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

  (use-package! org-modern
	:hook (org-mode . org-modern-mode)
	:config
	(setq
	 org-modern-star '( "⌾" "✸" "◈" "◇")
	 org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
	 org-modern-tag nil
	 org-modern-priority nil
	 org-modern-todo nil
	 org-modern-table nil))

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

  (use-package! apheleia :config
    ;; Setup Prettier
    (setf (alist-get 'prettier apheleia-formatters)
          '(npx "prettier"
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

(use-package! yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.config/doom/snippets")))
  (yas-reload-all)

(use-package! ef-themes

  :init
  (setq ef-themes-to-toggle '(ef-frost ef-dark))
  (map! :leader
        :desc "Toggle Ef Themes" "te" 'ef-themes-toggle)
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))

  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

  (setq ef-theme-region '(intense neutral))

  ;; Disable all other themes to avoid awkward blending
  (mapc #'disable-theme custom-enabled-themes)

  (ef-themes-select 'ef-dark)
  )

(map! :leader
      :after evil
      :desc "Ace Swap Windows" "wa" #'ace-swap-window)

(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))
