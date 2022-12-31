(setq user-full-name "Richard Cool"
      user-mail-address "richardjcool@gmail.com")

(setq-default delete-by-moving-to-trash t
              trash-directory "~/.local/share/Trash/files/")

(setq doom-font (font-spec :family "Anonymice Nerd Font Mono" :size 20)
      doom-big-font (font-spec :family "Anonymice Nerd Font Mono" :size 30)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 20)
      doom-unicode-font (font-spec :family "Spleen 32x64" :size 20)
      doom-serif-font (font-spec :family "DankMono Nerd Font" :size 20)

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

(after! org-mode
 (use-package! org-auto-mode
   :defer t
   :hook (org-mode . org-auto-tangle-mode)
   :config
   (setq org-auto-tangle-default t))
)

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

(after! doom-modeline
    (setq
     doom-modeline-hud t
     doom-modeline-minor-modes nil
          doom-modeline-height 25))

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

(defun my/name-of-buffers (n)
  "Return the names of the first N buffers from `buffer-list'."
  (let ((bns
         (delq nil
               (mapcar
                (lambda (b)
                  (unless (string-match "^ " (setq b (buffer-name b)))
                    b))
                (buffer-list)))))
    (subseq bns 1 (min (1+ n) (length bns)))))

;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
(defun my/number-names (list)
  "Enumerate and concatenate LIST."
  (let ((i 0))
    (mapconcat
     (lambda (x)
       (format "%d. %s" (cl-incf i) x))
     list
     ", ")))

(defvar my/last-buffers nil)

(defun my/switch-to-buffer (arg)
  (interactive "p")
  (switch-to-buffer
   (nth (1- arg) my/last-buffers)))

(defun my/switch-to-buffer-other-window (arg)
  (interactive "p")
  (switch-to-buffer-other-window
   (nth (1- arg) my/last-buffers)))

 (defhydra hydra:switch-buffer (:exit t
                                :body-pre (setq my/last-buffers
                                                (my/name-of-buffers 4)))
   "
_o_ther buffers: %s(my/number-names my/last-buffers)

"
   ("o" my/switch-to-buffer "this window")
   ("O" my/switch-to-buffer-other-window "other window")
   ("<escape>" nil))

(map!
 :leader
 :prefix "H"
(:desc "Themes"
       :nvm "t" #'hydra:themes/body)
(:desc "Windows"
       :nvm "w" #'hydra:windows/body)
(:desc "Dired"
       :nvm "d" #'hydra:dired/body)
(:desc "FlyCheck"
       :nvm "f" #'hydra:flycheck/body)
(:desc "Org"
       :nvm "o" #'hydra:org/body)
(:desc "Avy"
       :nvm "a" #'hydra:yasnippet/body)
(:desc "Folding"
       :nvm "z" #'hydra:folding/body)
(:desc "LSP"
       :nvm "l" #'hydra:lsp/body)
(:desc "Buffers"
       :nvm "b" #'hydra:buffers/body)
(:desc "DAP"
       :nvm "D" #'hydra-dap))
