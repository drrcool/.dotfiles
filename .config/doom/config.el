(setq user-full-name "Richard Cool"
      user-mail-address "richardjcool@gmail.com")

(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

(setq-default delete-by-moving-to-trash t
              trash-directory "~/.local/share/Trash/files/")

(setq doom-font (font-spec :family "PragmataPro Liga" :size 20)
      doom-big-font (font-spec :family "PragmataPro Liga" :size 30)
    doom-variable-pitch-font (font-spec :family "Fantasque Sans Mono" :size 20)
    doom-unicode-font (font-spec :family "PragmataPro Mono Liga" :size 20)
    doom-serif-font (font-spec :family "Rockwell" :size 20)

    )


(custom-set-faces!
'(font-lock-comment-face :slant italic)
'(font-lock-keyword-face :slant italic))

(use-package! modus-themes
:init
(setq modus-themes-hl-line '(accented intense)
    modus-themes-subtle-line-numbers t
    modus-themes-region '(accented bg-only)  ;; accented
    modus-themes-variable-pitch-ui t
    modus-themes-fringes 'intense
    modus-themes-diffs nil
    modus-themes-italic-constructs t
    modus-themes-bold-constructs t
    modus-themes-intense-mouseovers t
    modus-themes-paren-match '(bold intense)
    modus-themes-syntax '(alt-syntax yellow-comments green-strings)
    modus-themes-links '(neutral-underline background)
    modus-themes-mode-line '(borderless padded accented)
    modus-themes-tabs-accented nil ;; default
    modus-themes-completions
    '((matches . (extrabold intense accented))
        (selection . (semibold accented intense))
        (popup . (accented)))
    modus-themes-headings '((1 . (rainbow 1.4))
                            (2 . (rainbow 1.3))
                            (3 . (rainbow 1.2))
                            (4 . (rainbow bold 1.1))
                            (t . (rainbow bold)))
    modus-themes-org-blocks 'tinted-background
    modus-themes-org-agenda
    '((header-block . (semibold 1.4))
        (header-date . (workaholic bold-today 1.2))
        (event . (accented italic varied))
        (scheduled . rainbow)
        (habit . traffic-light))
    modus-themes-markup '(intense background)
    modus-themes-mail-citations 'intensep
    modus-themes-lang-checkers '(background))
)

:config
(modus-themes-load-vivendi)
(map! :leader
    :prefix "t" ;; toggle hrr
    :desc "Toggle Modus theme" "m" #'modus-themes-toggle)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; (god-mode)

(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)

(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)

(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶")
        org-modern-table-vertical 2
        org-modern-table-horizontal 4
        org-modern-list '((43 , "➤") (45 . "–") (42 . "•"))
        org-modern-footnote (cons nil (cadr org-script-display))
        org-modern-priority t
        org-modern-block t
        org-modern-block-fringe nil
        org-modern-horizontal-rule t
        ord-modern-keyword
        '((t                                                            .t)
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

(setq org-journal-dir "~/nc/Org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

(after! org
  (setq org-directory "~/org"
        org-agenda-files '("~/org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-table-convert-region-max-lines 20000
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "BLOG(b)"           ; Blog writing assignments
             "PROJ(p)"           ; A project that contains other tasks
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(setq ivy-posframe-display-functions-alist
      '((swiper                         . ivy-posframe-display-at-point)
     (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1)

(after! org
  ;; TODO: extract org-directory into noweb reference so it can be used in all the places
  (setq org-directory "~/org"
        org-agenda-files '(
                           "~/org/todo.org"
                           "~/org/agenda.org"
                           )
        +org-capture-todo-file "inbox.org")
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "HOLD(h)" "|" "DONE(d!)" "CANCELLED(c!)")))

  (setq org-todo-keyword-faces '(("TODO" . (:foreground "#7bc275" :weight normal))
                                 ("WAIT" . (:foreground "orange" :weight normal))
                                 ("HOLD" . (:inherit warning :weight normal))))

  (custom-set-faces!
    '(org-level-1 :inherit outline-1 :extend t :weight normal)
    '(org-level-2 :inherit outline-2 :extend t :weight normal)
    '(org-level-3 :inherit outline-3 :extend t :weight normal)
    '(org-level-4 :inherit outline-4 :extend t :weight normal)
    '(org-scheduled-today :foreground "#fcce7b") ;; warning yellow
    '(org-scheduled-previously :foreground "#ff665c") ;; error red
    '(org-imminent-deadline :foreground "#ff665c")
    '(org-upcoming-deadline :foreground "#fcce7b")
    '(org-checkbox-statistics-todo :inherit org-todo :weight normal)
    '(org-headline-todo :inherit org-level-2)
    )
  (add-hook 'org-mode-hook 'mixed-pitch-mode)
  (setq org-fontify-todo-headline nil)
  (setq org-archive-location "~/Things/archive/%s_archive::"
        org-refile-targets '(("~/Things/todo.org" :maxlevel . 2)
                             ("~/Things/someday.org" :maxlevel . 2)
                             ("~/Things/bookmarks.org" :level . 0))
        )
  (setq org-capture-templates
        '(
          ("t" "todo" entry (file +org-capture-todo-file) "* TODO %?")
          ("n" "node" entry (file +org-capture-todo-file) "* Note: ")
          ("p" "process email" entry (file +org-capture-todo-file)
           "* TODO %? %:fromname: %a")
          ))
  (setq org-goto-interface 'outline-path-completion
        org-outline-path-complete-in-steps nil)
  (defun make-link-to-pull-request (pull_no)
    (browse-url (concat "https://github.com/dbdrive/triebwerk/pull/" pull_no)))

  (defun make-link-to-issue (issue_no)
    (browse-url (concat "https://github.com/dbdrive/triebwerk/issues/" issue_no)))

  (org-add-link-type "pr" #'make-link-to-pull-request)
  (org-add-link-type "issue" #'make-link-to-issue)
  (setq org-table-duration-hour-zero-padding nil)
  (setq org-agenda-start-day nil                ;; start today
        org-agenda-span 'day                    ;; and show only today
        org-agenda-dim-blocked-tasks 'invisible ;; Don't show me any blocked todos. Next actions only - doesn't work for tags searches
        org-agenda-todo-ignore-scheduled 'future
        org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                      (todo scheduled-up todo-state-down priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep)))
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands
        '(
          ("n" "This sprint or other urgent matters"
           (
            (agenda "")
            ;; TODO: don't show done items
            (tags-todo "sprint+current" (
                                         (org-agenda-overriding-header "")
                                         (org-super-agenda-groups
                                          '(
                                            (:name "Talk!"
                                             :tag "agenda"
                                             :order 5)
                                            (:name "Waiting"
                                             :todo "WAIT"
                                             :order 9)
                                            (:name "Leftovers"
                                             :tag "leftover"
                                             :order 99)
                                            (:name "Tasks"
                                             :anything t
                                             :order 0)
                                            ))
                                         ))
            )
           )
          ))
  (setq org-agenda-hide-tags-regexp ".*") ;; Hide all tags in agenda view
  (setq org-agenda-block-separator 9472)     ;; Separate agenda and todos by a straight line
  (setq org-agenda-skip-scheduled-if-done t) ;; Don't show done items in calendar
  (setq org-agenda-entry-types '(:deadline :scheduled :timestamp :sexp)) ;; This is the default value
  (setq org-agenda-skip-deadline-if-done t) ;; Don't show done items in agenda
  (after! org (setq org-re-reveal-title-slide nil))
  )

(setq org-huge-base-dir "~/org/markdown")

(use-package autothemer

  :ensure t)

(load-theme 'doom-catppuccin t)
;; (load-theme 'catppuccin-latte t)
;; (load-theme 'catppuccin-macchiato t)
;; (load-theme 'catppuccin-mocha t)
;; (load-theme 'catppuccin t)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(setq tramp-default-method "ssh")
