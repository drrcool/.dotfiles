(setq mac-command-modifier       'meta
      mac-option-modifier        'meta
      mac-control-modifier       'control
      mac-right-option-modifier     'meta
      mac-right-control-modifier  'control)

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

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (counsel-ibuffer))

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

(setq doom-font (font-spec :family "PragmataProMonoLiga Nerd Font" :size 20)
      doom-big-font (font-spec :family "Iosevka Aile" :size 30)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 20)
      doom-unicode-font (font-spec :family "Spleen 32x64" :size 20)
      doom-serif-font (font-spec :family "Iosevka Aile" :size 20)

      )


(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(setq tramp-default-method "ssh")

(use-package! org-auto-mode
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

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
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window)

  (setq org-capture-templates
        '(
          ("t" "todo" entry (file +org-capture-todo-file) "* TODO %?")
          ("n" "node" entry (file +org-capture-todo-file) "* Note: ")
          ("p" "process email" entry (file +org-capture-todo-file)
           "* TODO %? %:fromname: %a")
          )))

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

(winner-mode +1)
(setq display-buffer-base-action
  '((display-buffer-reuse-window
     display-buffer-reuse-mode-window
     display-buffer-same-window
     display-buffer-in-previous-window)))

(after! doom-modeline
    (setq
     doom-modeline-hud t
     doom-modeline-minor-modes nil
          doom-modeline-height 25))

(use-package! mixed-pitch
  :defer t
  :config
  (setq mixed-pitch-set-height nil)
  (dolist (face '(org-date org-priority org-tag org-special-keyword))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
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
    "cqs" '(quickrun-select :which-key "Select backend")"cq" '(nil :which-key "quickrun")
    "cqq" '(quit-window :which-key "Quit")
    "cqr" '(quickrun :which-key "Run")
    "cqR" '(quickrun-region :which-key "Run Region")
    "cqa" '(quickrun-with-arg :which-key "Run with [A]rgs")
    "cqm" '(quickrun-autorun-mode :which-key "Toggle autorun mode")
    "cqs" '(quickrun-select :which-key "Select backend")
))

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

(use-package! tsx-mode

  :hook (tsx-mode . lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . tsx-mode))

(map!
 :leader
 :prefix "m"
 :map 'general-override-mode-map
 (:desc "Toggle All Nodes"
  :nmv #'tsx-mode-fold-toggle-all-nodes)
 (:desc "Toggle Coverage"
        :nmv #'tsx-mode-coverage-toggle)
 (:desc "Toggle Node"
        :nvm #'tsx-mode-fold-toggle-node)
)
)

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

(general-def
  :prefix-map 'rc/avy-map
"c" #'avy-goto-char-timer
"C" #'avy-goto-char
"w" #'avy-goto-word-1
"W" #'avy-goto-word-0
"l" #'avy-goto-line
"L" #'avy-goto-end-of-line
"m" #'avy-move-line
"M" #'avy-move-region
"k" #'avy-kill-whole-line
"K" #'avy-kill-region
"y" #'avy-copy-line
"Y" #'avy-copy-region
)
(hercules-def
 :toggle-funs #'rc/avy-mode
 :keymap 'rc/avy-map
 :transient t
)
(map!
 :leader
 :prefix "H"
 :desc "Avy"
 :nm "a" #'rc/avi-mode)

;; (general-def
;;   :prefix-map 'rc/snippet-map
;;   "d" #'yas-load-directory
;;   "e" #'yas-activate-extra-mode
;;   "i" #'yas-insert-snippet
;;   "f" #'yas-visit-snippet-file
;;   "n" #'yas-new-snippet
;;   "t" #'yas-tryout-snippet
;;   "l" #'yas-describe-tables
;;   "g" #'yas/global-mode
;;   "m" #'yas/minor-mode
;;   "a" #'yas-reload-all
;; )
;; (hercules-def
;;  :toggle-funs #'rc/snippet-mode
;;  :keymap 'rc/snippet-map
;;  :transiet t)

;; (map!
;;  :leader
;;  :prefix "H"
;;  :desc "Snippet"
;;  :nm "s" #'rc/snippet-mode)

(general-def
  :prefix-map 'rc/origami-map
   "c" #'origami-close-node
   "n" #'origami-next-fold
   "p" #'origami-previous-fold
   "f" #'origami-forward-toggle-node
   "a" #'origami-toggle-all-nodes
   "s" #'origami-show-only-node
   )
(hercules-def
 :toggle-funs #'rc/origami-mode
 :keymap 'rc/origami-map
 :transient t)
(map!
:leader
:prefix "H"
:desc "Folding"
:nm "f" #'rc/origami-mode
)

(hercules-def
 :toggle-funs #'rc/magit-map
 :keymap 'magit-mode-map
 :transient t)
(hercules-def
 :toggle-funs #'rc/buffer-mode
 :keymap 'doom-leader-buffer-map
:transient t)

(map!
 :leader
 :desc "Buffers"
 :nm "b" #'rc/buffer-mode)

(general-def
  :prefix-map 'my-dired-map

  "\\" #'dired-do-ispell
  "(" #'dired-hide-details-mode
  ")" #'dired-omit-mode
  "+" #'dired-create-directory
  "=" #'diredp-ediff         ;; smart diff
  "?" #'dired-summary
  "$" #'diredp-hide-subdir-nomove
  "A" #'dired-do-find-regexp
  "C" #'dired-do-copy        ;; Copy all marked files
  "D" #'dired-do-delete
  "E" #'dired-mark-extension
  "e" #'dired-ediff-files
  "F" #'dired-do-find-marked-files
  "G" #'dired-do-chgrp
  "g" #'revert-buffer        ;; read all directories again (refresh)
  "i" #'dired-maybe-insert-subdir
  "l" #'dired-do-redisplay   ;; relist the marked or singel directory
  "M" #'dired-do-chmod
  "m" #'dired-mark
  "O" #'dired-display-file
  "o" #'dired-find-file-other-window
  "Q" #'dired-do-find-regexp-and-replace
  ;; "R" #'dired-do-rename
  ;; "r" #'dired-do-rsynch
  ;; "S" #'dired-do-symlink
  ;; "s" #'dired-sort-toggle-or-edit
  ;; "t" #'dired-toggle-marks
  ;; "U" #'dired-unmark-all-marks
  ;; "u" #'dired-unmark
  ;; "v" #'dired-view-file      ;; q to exit, s to search, = gets line #
  ;; "w" #'dired-kill-subdir
  ;; "Y" #'dired-do-relsymlink
  "z" #'diredp-compress-this-file
  "Z" #'dired-do-compress
  )

(hercules-def
 :toggle-funs #'my-dired-mode
 :keymap 'my-dired-map
 :transient t)

(map!
 :leader
 :desc "Hercules"
 :"H" 'nil)

(map!
 :leader
 :prefix "H"
 (:desc "Magit"
 :nm "m" #'rc/magit-map)
(:desc "Dired"
 :nm "d" #'my-dired-mode)
                )
