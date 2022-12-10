(setq user-full-name "Richard Cool"
      user-mail-address "richardjcool@gmail.com")

(cond (IS-MAC
       (setq
             mac-right-option-modifier  'alt
             mac-option-modifier        'alt
             mac-pass-control-to-system nil)))

(setq-default window-combination-resize t)

(setq-default delete-by-moving-to-trash t
              trash-directory "~/.local/share/Trash/files/")

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-bufferr (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; Increase undo history limits even more
(after! undo-fu
  ;; Emacs undo defaults
  (setq undo-limit        10000000    ;; 1MB   (default is 160kB, Doom's default is 400kB)
        undo-strong-limit 100000000   ;; 100MB (default is 240kB, Doom's default is 3MB)
        undo-outer-limit  1000000000) ;; 1GB   (default is 24MB,  Doom's default is 48MB)

  ;; Undo-fu customization options
  (setq undo-fu-allow-undo-in-region t ;; Undoing with a selection will use undo within that region.
        undo-fu-ignore-keyboard-quit t)) ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.

;; Evil undo
(after! evil
  (setq evil-want-fine-undo t)) ;; By default while in insert all changes are one big blob

(use-package! vundo
  :defer t
  :init
  (defconst +vundo-unicode-symbols
    '((selected-node   . ?●)
      (node            . ?○)
      (vertical-stem   . ?│)
      (branch          . ?├)
      (last-branch     . ?╰)
      (horizontal-stem . ?─)))

  (map! :leader
        (:prefix ("o")
         :desc "vundo" "v" #'vundo))

  :config
  (setq vundo-glyph-alist +vundo-unicode-symbols
        vundo-compact-display t
        vundo-window-max-height 6))

;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

;; Enable relative line numbers
(setq display-line-numbers-type 'relative)

;; Iterate through CamelCase words
(global-subword-mode 1)

(when (daemonp)
  (add-hook! '(delete-frame-functions delete-terminal-functions)
(let ((inhibit-message t))
    (recentf-save-list)
    (savehist-save))))

(setq doom-font (font-spec :family "Iosevka Fixed Curly Slab" :size 20)
      doom-big-font (font-spec :family "Iosevka Fixed Curly Slab" :size 30)
    doom-variable-pitch-font (font-spec :family "Fantasque Sans Mono")
    doom-unicode-font (font-spec :family "JuliaMono")
    doom-serif-font (font-spec :family "Iosevka Fixed Curly Slab")
    )



(custom-set-faces!
'(font-lock-comment-face :slant italic)
'(font-lock-keyword-face :slant italic))

(use-package! modus-themes
:init
(setq modus-themes-hl-line '(accented intense)
    modus-themes-subtle-line-numbers t
    modus-themes-region '(bg-only no-extend) ;; accented
    modus-themes-variable-pitch-ui nil
    modus-themes-fringes 'subtle
    modus-themes-diffs nil
    modus-themes-italic-constructs t
    modus-themes-bold-constructs t
    modus-themes-intense-mouseovers t
    modus-themes-paren-match '(bold intense)
    modus-themes-syntax '(green-strings)
    modus-themes-links '(neutral-underline background)
    modus-themes-mode-line '(borderless padded)
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

(defun +modus-themes-tweak-packages ()
(modus-themes-with-colors
    (set-face-attribute 'cursor nil :background (modus-themes-color 'blue))
    (set-face-attribute 'font-lock-type-face nil :foreground (modus-themes-color 'magenta-alt))
    (custom-set-faces
    ;; Tweak `evil-mc-mode'
    `(evil-mc-cursor-default-face ((,class :background ,magenta-intense-bg)))
    ;; Tweak `git-gutter-mode'
    `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
    `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
    `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg)))
    ;; Tweak `doom-modeline'
    `(doom-modeline-evil-normal-state ((,class :foreground ,green-alt-other)))
    `(doom-modeline-evil-insert-state ((,class :foreground ,red-alt-other)))
    `(doom-modeline-evil-visual-state ((,class :foreground ,magenta-alt)))
    `(doom-modeline-evil-operator-state ((,class :foreground ,blue-alt)))
    `(doom-modeline-evil-motion-state ((,class :foreground ,blue-alt-other)))
    `(doom-modeline-evil-replace-state ((,class :foreground ,yellow-alt)))
    ;; Tweak `diff-hl-mode'
    `(diff-hl-insert ((,class :foreground ,green-fringe-bg)))
    `(diff-hl-delete ((,class :foreground ,red-fringe-bg)))
    `(diff-hl-change ((,class :foreground ,yellow-fringe-bg)))
    ;; Tweak `solaire-mode'
    `(solaire-default-face ((,class :inherit default :background ,bg-alt :foreground ,fg-dim)))
    `(solaire-line-number-face ((,class :inherit solaire-default-face :foreground ,fg-unfocused)))
    `(solaire-hl-line-face ((,class :background ,bg-active)))
    `(solaire-org-hide-face ((,class :background ,bg-alt :foreground ,bg-alt)))
    ;; Tweak `display-fill-column-indicator-mode'
    `(fill-column-indicator ((,class :height 0.3 :background ,bg-inactive :foreground ,bg-inactive)))
    ;; Tweak `mmm-mode'
    `(mmm-cleanup-submode-face ((,class :background ,yellow-refine-bg)))
    `(mmm-code-submode-face ((,class :background ,bg-active)))
    `(mmm-comment-submode-face ((,class :background ,blue-refine-bg)))
    `(mmm-declaration-submode-face ((,class :background ,cyan-refine-bg)))
    `(mmm-default-submode-face ((,class :background ,bg-alt)))
    `(mmm-init-submode-face ((,class :background ,magenta-refine-bg)))
    `(mmm-output-submode-face ((,class :background ,red-refine-bg)))
    `(mmm-special-submode-face ((,class :background ,green-refine-bg))))))

(add-hook 'modus-themes-after-load-theme-hook #'+modus-themes-tweak-packages)

:config
(modus-themes-load-vivendi)
(map! :leader
    :prefix "t" ;; toggle
    :desc "Toggle Modus theme" "m" #'modus-themes-toggle))

(after! doom-modeline
(setq display-time-string-forms
    '((propertize (concat " 🕘 " 24-hours ":" minutes))))
(display-time-mode 1) ; Enable time in the mode-line

;; Add padding to the right
(doom-modeline-def-modeline 'main
'(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
'(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker "   ")))

(after! doom-modeline
(let ((battery-str (battery)))
(unless (or (equal "Battery status not available" battery-str)
            (string-match-p (regexp-quote "unknown") battery-str)
            (string-match-p (regexp-quote "N/A") battery-str))
    (display-battery-mode 1))))

(after! doom-modeline
(setq doom-modeline-bar-width 4
    doom-modeline-major-mode-icon t
    doom-modeline-major-mode-color-icon t
    doom-modeline-buffer-file-name-style 'truncate-upto-project))
;; NOTE: Not tangled
(set-frame-parameter (selected-frame) 'alpha '(95 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

(setq fancy-splash-image (expand-file-name "assets/doom-emacs-gray.svg" doom-user-dir))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-hook! '+doom-dashboard-mode-hook (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(setq which-key-idle-delay 0.05 ;; Default is 1.0
    which-key-idle-secondary-delay 0.05) ;; Default is nil
(setq which-key-allow-multiple-replacements t)

(after! which-key
(pushnew! which-key-replacement-alist
        '((""       . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "🅔·\\1"))
        '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)")       . (nil . "Ⓔ·\\1"))))

(setq frame-title-format
    '(""
    (:eval
        (if (s-contains-p org-roam-directory (or buffer-file-name ""))
            (replace-regexp-in-string ".*/[0-9]*-?" "☰ "
                                    (subst-char-in-string ?_ ?\s buffer-file-name))
        "%b"))
    (:eval
        (when-let* ((project-name (projectile-project-name))
                    (project-name (if (string= "-" project-name)
                                    (ignore-errors (file-name-base (string-trim-right (vc-root-dir))))
                                    project-name)))
        (format (if (buffer-modified-p) " ○ %s" " ● %s") project-name)))))

(use-package! focus
:commands focus-mode):

(use-package! good-scroll
:unless EMACS29+
:config (good-scroll-mode 1))

(when EMACS29+
(pixel-scroll-precision-mode 1))

(setq hscroll-step 1
    hscroll-margin 0
    scroll-step 1
    scroll-margin 0
    scroll-conservatively 101
    scroll-up-aggressively 0.01
    scroll-down-aggressively 0.01
    scroll-preserve-screen-position 'always
    auto-window-vscroll nil
    fast-but-imprecise-scrolling nil)

(after! all-the-icons
(setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(after! centaur-tabs
;; For some reason, setting `centaur-tabs-set-bar' this to `right'
;; instead of Doom's default `left', fixes this issue with Emacs daemon:
;; https://github.com/doomemacs/doomemacs/issues/6647#issuecomment-1229365473
(setq centaur-tabs-set-bar 'right
    centaur-tabs-gray-out-icons 'buffer
    centaur-tabs-set-modified-marker t
    centaur-tabs-close-button "⨂"
    centaur-tabs-modified-marker "⨀"))

(after! writeroom-mode
;; Show mode line
(setq writeroom-mode-line t)

;; Disable line numbers
(add-hook! 'writeroom-mode-enable-hook
(when (bound-and-true-p display-line-numbers-mode)
    (setq-local +line-num--was-activate-p display-line-numbers-type)
    (display-line-numbers-mode -1)))

(add-hook! 'writeroom-mode-disable-hook
(when (bound-and-true-p +line-num--was-activate-p)
    (display-line-numbers-mode +line-num--was-activate-p)))

(after! org
;; Increase latex previews scale in Zen mode
(add-hook! 'writeroom-mode-enable-hook (+org-format-latex-set-scale 2.0))
(add-hook! 'writeroom-mode-disable-hook (+org-format-latex-set-scale 1.4)))

(after! blamer
;; Disable blamer in zen (writeroom) mode
(add-hook! 'writeroom-mode-enable-hook
    (when (bound-and-true-p blamer-mode)
    (setq +blamer-mode--was-active-p t)
    (blamer-mode -1)))
(add-hook! 'writeroom-mode-disable-hook
    (when (bound-and-true-p +blamer-mode--was-active-p)
    (blamer-mode 1)))))

(after! highlight-indent-guides
(setq highlight-indent-guides-character ?│
    highlight-indent-guides-responsive 'top))

(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)

(after! evil
;; This fixes https://github.com/doomemacs/doomemacs/issues/6478
;; Ref: https://github.com/emacs-evil/evil/issues/1630
(evil-select-search-module 'evil-search-module 'isearch)

(setq evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

(use-package! aggressive-indent
:commands (aggressive-indent-mode))

(setq yas-triggers-in-field t)

(setq company-global-modes
      '(not erc-mode
             circe-mode
             help-mode
             gud-mode
             vterm-mode
             org-mode
             ))

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

(after! treemacs
  (require 'dired)

  ;; My custom stuff (from tecosaur's config)
  (setq +treemacs-file-ignore-extensions
        '(;; LaTeX
          "aux" "ptc" "fdb_latexmk" "fls" "synctex.gz" "toc"
          ;; LaTeX - bibliography
          "bbl"
          ;; LaTeX - glossary
          "glg" "glo" "gls" "glsdefs" "ist" "acn" "acr" "alg"
          ;; LaTeX - pgfplots
          "mw"
          ;; LaTeX - pdfx
          "pdfa.xmpi"
          ;; Python
          "pyc"))

  (setq +treemacs-file-ignore-globs
        '(;; LaTeX
          "*/_minted-*"
          ;; AucTeX
          "*/.auctex-auto"
          "*/_region_.log"
          "*/_region_.tex"
          ;; Python
          "*/__pycache__"))

  ;; Reload treemacs theme
  (setq doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  (setq treemacs-show-hidden-files nil
        treemacs-hide-dot-git-directory t
        treemacs-width 30)

  (defvar +treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")

  (defvar +treemacs-file-ignore-globs '()
    "Globs which will are transformed to `+treemacs-file-ignore-regexps' which `+treemacs-ignore-filter' will ensure are ignored")

  (defvar +treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `+treeemacs-file-ignore-globs'")

  (defun +treemacs-file-ignore-generate-regexps ()
    "Generate `+treemacs-file-ignore-regexps' from `+treemacs-file-ignore-globs'"
    (setq +treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp +treemacs-file-ignore-globs)))

  (unless (equal +treemacs-file-ignore-globs '())
    (+treemacs-file-ignore-generate-regexps))

  (defun +treemacs-ignore-filter (file full-path)
    "Ignore files specified by `+treemacs-file-ignore-extensions', and `+treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) +treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp +treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))

  (add-to-list 'treemacs-ignored-file-predicates #'+treemacs-ignore-filter))

;; Run `M-x projectile-discover-projects-in-search-path' to reload paths from this variable
(setq projectile-project-search-path
      '("~/PhD/papers"
        "~/PhD/workspace"
        "~/PhD/workspace-no"
        "~/PhD/workspace-no/ez-wheel/swd-starter-kit-repo"
        ("~/Projects/foss" . 2))) ;; ("dir" . depth)

(setq projectile-ignored-projects
      '("/tmp"
        "~/"
        "~/.cache"
        "~/.doom.d"
        "~/.emacs.d/.local/straight/repos/"))

(setq +projectile-ignored-roots
      '("~/.cache"
        ;; No need for this one, as `doom-project-ignored-p' checks for files in `doom-local-dir'
        "~/.emacs.d/.local/straight/"))

(defun +projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `+projectile-ignored-roots'"
  (require 'cl-lib)
  (or (doom-project-ignored-p filepath) ;; Used by default by doom with `projectile-ignored-project-function'
      (cl-some (lambda (root) (file-in-directory-p (expand-file-name filepath) (expand-file-name root)))
          +projectile-ignored-roots)))

(setq projectile-ignored-project-function #'+projectile-ignored-project-function)

(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 

(setq eros-eval-result-prefix "⟹ ")

(defun +dir-locals-reload-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun +dir-locals-reload-for-all-buffers-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (+dir-locals-reload-for-current-buffer))))))

(defun +dir-locals-enable-autoreload ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (message "Dir-locals will be reloaded after saving.")
    (add-hook 'after-save-hook '+dir-locals-reload-for-all-buffers-in-this-directory nil t)))

(after! eglot
  ;; A hack to make it works with projectile
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))

  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))

  ;; Use clangd with some options
  (set-eglot-client! 'c++-mode '("clangd" "-j=3" "--clang-tidy")))

(after! lsp-mode
  (setq lsp-idle-delay 1.0
        lsp-log-io nil
        gc-cons-threshold (* 1024 1024 100))) ;; 100MiB

(after! lsp-mode
  (setq lsp-lens-enable t
        lsp-semantic-tokens-enable t ;; hide unreachable ifdefs
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        ;; LSP UI related tweaks
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil))

                (after! tramp
  (when (require 'lsp-mode nil t)
    ;; (require 'lsp-pyright)

    (setq lsp-enable-snippet nil
          lsp-log-io nil
          ;; To bypass the "lsp--document-highlight fails if
          ;; textDocument/documentHighlight is not supported" error
          lsp-enable-symbol-highlighting nil)

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "pyls")
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pyls-remote))))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(after! dap-mode
  ;; Set latest versions
  (setq dap-cpptools-extension-version "1.11.5")
  (require 'dap-cpptools)

  (setq dap-codelldb-extension-version "1.7.4")
  (require 'dap-codelldb)

  (setq dap-gdb-lldb-extension-version "0.26.0")
  (require 'dap-gdb-lldb)

  ;; More minimal UI
  (setq dap-auto-configure-features '(breakpoints locals expressions tooltip)
        dap-auto-show-output nil ;; Hide the annoying server output
        lsp-enable-dap-auto-configure t)

  ;; Automatically trigger dap-hydra when a program hits a breakpoint.
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))

  ;; Automatically delete session and close dap-hydra when DAP is terminated.
  (add-hook 'dap-terminated-hook
            (lambda (arg)
              (call-interactively #'dap-delete-session)
              (dap-hydra/nil)))

  ;; A workaround to correctly show breakpoints
  ;; from: https://github.com/emacs-lsp/dap-mode/issues/374#issuecomment-1140399819
  (add-hook! +dap-running-session-mode
    (set-window-buffer nil (current-buffer))))

(defun +debugger/clear-last-session ()
  "Clear the last stored session"
  (interactive)
  (doom-store-clear "+debugger"))

(map! :leader :prefix ("l" . "custom")
      (:when (modulep! :tools debugger +lsp)
       :prefix ("d" . "debugger")
       :desc "Clear last DAP session" "c" #'+debugger/clear-last-session))

(setq emojify-emoji-set "twemoji-v2")

(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓" "⏱" "®" "™" "🅱" "❌" "✳"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))

(defun +appened-to-negation-list (head tail)
  (if (sequencep head)
      (delete-dups
       (if (eq (car tail) 'not)
           (append head tail)
         (append tail head)))
    tail))

(when (modulep! :ui ligatures)
  (setq +ligatures-extras-in-modes
        (+appened-to-negation-list
         +ligatures-extras-in-modes
         '(not c-mode c++-mode emacs-lisp-mode python-mode scheme-mode racket-mode rust-mode)))

  (setq +ligatures-in-modes
        (+appened-to-negation-list
         +ligatures-in-modes
         '(not emacs-lisp-mode scheme-mode racket-mode))))

(map! :leader
      (:when (modulep! :ui workspaces)
       :prefix ("TAB" . "workspace")
       :desc "Display tab bar"           "TAB" #'+workspace/display
       :desc "Switch workspace"          "."   #'+workspace/switch-to
       :desc "Switch to last workspace"  "$"   #'+workspace/other ;; Modified
       :desc "New workspace"             "n"   #'+workspace/new
       :desc "New named workspace"       "N"   #'+workspace/new-named
       :desc "Load workspace from file"  "l"   #'+workspace/load
       :desc "Save workspace to file"    "s"   #'+workspace/save
       :desc "Delete session"            "x"   #'+workspace/kill-session
       :desc "Delete this workspace"     "d"   #'+workspace/delete
       :desc "Rename workspace"          "r"   #'+workspace/rename
       :desc "Restore last session"      "R"   #'+workspace/restore-last-session
       :desc "Next workspace"            ">"   #'+workspace/switch-right ;; Modified
       :desc "Previous workspace"        "<"   #'+workspace/switch-left ;; Modified
       :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
       :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
       :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
       :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
       :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
       :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
       :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
       :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
       :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
       :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))

(use-package! tldr
  :commands (tldr-update-docs tldr)
  :init
  (setq tldr-enabled-categories '("common" "linux" "osx" "sunos")))

(after! evil
  (evil-define-key 'insert fzf-mode-map (kbd "ESC") #'term-kill-subjob))

(define-minor-mode fzf-mode
  "Minor mode for the FZF buffer"
  :init-value nil
  :lighter " FZF"
  :keymap '(("C-c" . term-kill-subjob)))

(defadvice! doom-fzf--override-start-args-a (original-fn &rest args)
  "Set the FZF minor mode with the fzf buffer."
  :around #'fzf/start
  (message "called with args %S" args)
  (apply original-fn args)

  ;; set the FZF buffer to fzf-mode so we can hook ctrl+c
  (set-buffer "*fzf*")
  (fzf-mode))

(defvar fzf/args
  "-x --print-query -m --tiebreak=index --expect=ctrl-v,ctrl-x,ctrl-t")

(use-package! fzf
  :commands (fzf fzf-projectile fzf-hg fzf-git fzf-git-files fzf-directory fzf-git-grep))

(after! code-review
  (setq code-review-auth-login-marker 'forge))
(after! magit
  ;; Disable if it causes performance issues
  (setq magit-diff-refine-hunk t))

(use-package! magit-pretty-graph
  :after magit
  :init
  (setq magit-pg-command
        (concat "git --no-pager log"
                " --topo-order --decorate=full"
                " --pretty=format:\"%H%x00%P%x00%an%x00%ar%x00%s%x00%d\""
                " -n 2000")) ;; Increase the default 100 limit

  (map! :localleader
        :map (magit-mode-map)
        :desc "Magit pretty graph" "p" (cmd! (magit-pg-repo (magit-toplevel)))))

(after! org
(setq org-directory "~/org/" ; let's put files here
      org-use-property-inheritance t ; it's convenient to have properties inherited
      org-log-done 'time             ; having the time an item is done sounds convenient
      org-list-allow-alphabetical t  ; have a. A. a) A) list bullets
      org-export-in-background nil   ; run export processes in external emacs process
      org-export-async-debug t
      org-tags-column 0
      org-catch-invisible-edits 'smart ;; try not to accidently do weird stuff in invisible regions
      org-export-with-sub-superscripts '{} ;; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
      org-pretty-entities-include-sub-superscripts nil
      org-auto-align-tags nil
      org-special-ctrl-a/e t
      org-startup-indented t ;; Enable 'org-indent-mode' by default, override with '+#startup: noindent' for big files
      org-insert-heading-respect-content t)

                (setq org-babel-default-header-args
      '((:session  . "none")
        (:results  . "replace")
        (:exports  . "code")
        (:cache    . "no")
        (:noweb    . "no")
        (:hlines   . "no")
        (:tangle   . "no")
        (:comments . "link")))
(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>" #'org-backward-heading-same-level
      :n "g <down>" #'org-forward-heading-same-level
      :n "g <left>" #'org-up-element
      :n "g <right>" #'org-down-element)
)

(after! org

(setq org-todo-keywords
      '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "goldenrod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))
        ("STRT" . (:foreground "OrangeRed" :weight bold))
        ("WAIT" . (:foreground "coral" :weight bold))
        ("KILL" . (:foreground "DarkGreen" :weight bold))
        ("PROJ" . (:foreground "LimeGreen" :weight bold))
        ("HOLD" . (:foreground "orange" :weight bold))))

(defun +log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'+log-todo-next-creation-date)
  )

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("home"      . ?h)
        ("research"  . ?r)
        ("work"      . ?w)
        (:endgroup   . nil)
        (:startgroup . nil)
        ("tool"      . ?o)
        ("dev"       . ?d)
        ("report"    . ?p)
        (:endgroup   . nil)
        (:startgroup . nil)
        ("easy"      . ?e)
        ("medium"    . ?m)
        ("hard"      . ?a)
        (:endgroup   . nil)
        ("urgent"    . ?u)
        ("key"       . ?k)
        ("bonus"     . ?b)
        ("ignore"    . ?i)
        ("noexport"  . ?x)))

(setq org-tag-faces
      '(("home"     . (:foreground "goldenrod"  :weight bold))
        ("research" . (:foreground "goldenrod"  :weight bold))
        ("work"     . (:foreground "goldenrod"  :weight bold))
        ("tool"     . (:foreground "IndianRed1" :weight bold))
        ("dev"      . (:foreground "IndianRed1" :weight bold))
        ("report"   . (:foreground "IndianRed1" :weight bold))
        ("urgent"   . (:foreground "red"        :weight bold))
        ("key"      . (:foreground "red"        :weight bold))
        ("easy"     . (:foreground "green4"     :weight bold))
        ("medium"   . (:foreground "orange"     :weight bold))
        ("hard"     . (:foreground "red"        :weight bold))
        ("bonus"    . (:foreground "goldenrod"  :weight bold))
        ("ignore"   . (:foreground "Gray"       :weight bold))
        ("noexport" . (:foreground "LimeGreen"  :weight bold))))

(after! org

(setq org-agenda-files
      (list (expand-file-name "inbox.org" org-directory)
            (expand-file-name "agenda.org" org-directory)
            (expand-file-name "gcal-agenda.org" org-directory)
            (expand-file-name "notes.org" org-directory)
            (expand-file-name "projects.org" org-directory)
            (expand-file-name "archive.org" org-directory)))
;; Agenda styling
(setq org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")

)

(after! org
(use-package! org-super-agenda
  :defer t
  :config
  (org-super-agenda-mode)
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-tags-column 100 ;; from testing this seems to be a good value
        org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :todo "TODAY"
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do" :todo "NEXT" :order 1)
                            (:name "Important" :tag "Important" :priority "A" :order 6)
                            (:name "Due Today" :deadline today :order 2)
                            (:name "Due Soon" :deadline future :order 8)
                            (:name "Overdue" :deadline past :face error :order 7)
                            (:name "Assignments" :tag "Assignment" :order 10)
                            (:name "Issues" :tag "Issue" :order 12)
                            (:name "Emacs" :tag "Emacs" :order 13)
                            (:name "Projects" :tag "Project" :order 14)
                            (:name "Research" :tag "Research" :order 15)
                            (:name "To read" :tag "Read" :order 30)
                            (:name "Waiting" :todo "WAIT" :order 20)
                            (:name "University" :tag "Univ" :order 32)
                            (:name "Trivial" :priority<= "E" :tag ("Trivial" "Unimportant") :todo ("SOMEDAY") :order 90)
                            (:discard (:tag ("Chore" "Routine" "Daily"))))))))))))
)

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
      org-hide-leading-stars t)
      ;; org-priority-highest ?A
      ;; org-priority-lowest ?E
      ;; org-priority-faces
      ;; '((?A . 'all-the-icons-red)
      ;;   (?B . 'all-the-icons-orange)
      ;;   (?C . 'all-the-icons-yellow)
      ;;   (?D . 'all-the-icons-green)
      ;;   (?E . 'all-the-icons-blue)))
