(setq straight-vs-git-default-clone-depth 1)
(setq straight-recipes-gnu-ela-use-mirror t)
(setq straight-check-for-modifications nil)
(setq use-package-always-defer t)

;; Bootstrap straight
(defvar boostrap-version)
(let* ((straight-repo-dir
        (expand-file-name "straight/repos" user-emacs-directory))
       (bootstrap-file
        (concat straight-repo-dir "/straight.el/bootstrap.el"))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (concat
      "mkdir -p " straight-repo-dir " && "
      "git -C " straight-repo-dir " clone "
      "https://github.com/raxod502/straight.el.git && "
      "git -C " straight-repo-dir "checkout 2d407bc")))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq comp-deferred-compilation-black-list nil)

;;Save statitistics about how many packages you have loaded
(setq use-package-compute-statistics t)

;; Set some sane defaults
(use-package emacs
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        frame-resize-pixelwise t)

  (setq user-fill-name "Richard Cool"
        user-mail-address "richardjcool@gmail.com")

  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; Always allow y instead of yes
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; default to utf-8 for all things
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; Write over selected text on input
  (delete-selection-mode t)

  ;; enable recent files mode
  (recentf-mode t)
  (setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory)
                          ,(expand-file-name "etc/" user-emacs-directory)
                          ,(expand-file-name "var/" user-emacs-direcotry)))

  ;; use escape to cancel
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Don't persist a custom file
  (setq custom-file (make-temp-file ""))   ; use a temp file as a placeholder
  (setq custom-safe-themes t)       ; mark all themes as safe since we can't persist now
  (setq enable-local-variables :all)  ; fix =defvar= warnings

  ;; stop emacs from littering the file system with backups
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; follow symlinks
  (setq vc-follow-symlinks t)

  ;; Don't show extra window chrome
  (when (window-system)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1))


  ;; Enable winner mode globally
  (winner-mode t)

  (show-paren-mode t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; Clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)

  ;; use common convention for indenting by default
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 2)

  ;; Enable indentiation + completion using the TAB key.
  ;; Completion is often bound to M-tab
  (setq tab-always-indent 'complete)
  )

;; set some custom variables
(use-package emacs
  :init

  (defcustom rc/default-font-family "Spleen32x64 Nerd Font"
    "Default font family"
    :type 'string
    :group 'rc)

  (defcustom rc/variable-pitch-font-family "Fantasque Sans Mono"
    "Variable pitch font family"
    :type 'string
    :group 'rc)

  (defcustom rc/monitor-font-size
    200
    "Font size used for mac (assuming external display)"
    :type 'int
    :group 'rc)

  (defcustom rc/laptop-font-size
    150
    "Font size used on laptop"
    :type 'int
    :group 'rc)

  (defcustom rc/theme nil
    "Current theme"
    :type 'symbol
    :options '(light-dark)
    :group 'rc)

  )

;; Set Fonts
(use-package emacs
  :hook (after-init . rc/set-font-size)
  :init
  (defun rc/get-font-size ()
    "Font size is calculated according to the size of the primary screen"
    (let* (
           (command "osascript -e 'tell application \"finder\" to get bounds of window of desktop' | cut -d',' -f3")
           (screen-width (string-to-number (shell-command-to-string command))))
      (if (> screen-window 2560) rc/monitor-font-size lc/laptop-font-size)))
  (defun rc/set-font-size ()
    (interactive)
    ;; Main typeface
    (set-face-attribute 'default nil :family rc/default-font-famioy :height (rc/get-font-size))
    ;; set fixed pitch face
    (set-face-attribute 'default nil :family rc/default-font-family)
    ;; set the variable pitch face
    (set-face-attribute 'variable-pitch nil :family rc/variable-pitch-font-family)
    ;; modeline
    (set-face-attribute 'mode-line nil :family rc/default-font-family :height (rc/get-font-size))
    (set-face-attribute 'mode-line-inactive nil :family rc/default-font-family :height (rc/get-font-size))
    )
  )

(use-package emacs
  :init
  (defun rc/adjust-font-size (height)
    "Adjust font size by given height. If height is '0' reset font size. This function also handles icons and model line font sizes"
    (interactive "nHeight ('0' to reset): ")
    (let ((new-height (if (zerop height)
                          (rc/get-font-size)
                        (+ height (face-attribute 'default :height)))))
      (set-face-attribute 'default nil :height new-height)
      (set-face-attribute 'fixed-pitch nil :height new-height)
      (set-face-attribute 'variable-pitch nil :height new-height)
      (set-face-attribute 'mode-line-inactive nil :height new-height)
      (message "Font size: %s" new-height)))

  (defun rc/increase-font-size ()
    "increase font size by 0.5 (5 in height)"
    (interactive)
    (rc/adjust-font-size 5))

  (defun rc/decrease-font-size ()
    "decrease font size by 0.5 (5 in height)"
    (interactive)
    (rc/adjust-font-size (-5)))

  (defun rc/reset-font-size ()
    "Reset font size accoring to the rc/default-font-size."
    (interactive)
    (rc/adjust-font-size 0))
  )

;; Zoom
(use-package emacs
  :init
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  )


;; Some mac config
(use-package emacs
  :init
  (defun rc/is-macos ()
    (and (eq system-type 'darwin)
         (= 0 (length (shell-command-to-string "uname -a | grep iPad")))))
  (when (rc/is-macos)
    (setq mac-right-command-modifier 'control
          mac-option-modifier 'meta
          mac-control-modifier 'meta
          )
    )
    ;; configure kill ring
    (when (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode)
      (global-set-key [(s c)] 'kill-ring-save)
      (global-set-key [(s v)] 'yank)
      (global-set-key [(s x)] 'kill-region)
      (global-set-key [(s q)] 'kill-emacs)
      ))

;; Manage garbage collection like doom
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; helpful help docs
(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; setup eldoc
(use-package eldoc
  :hook (emacs-lisp-mode cider-mode))

;; Execute path from shell
(use-package exec-path-from-shell
  :if (rc/is-macos)
  :hook (emacs-startup . (lambda ()
                           (setq exec-path-from-shell-arguments '("-l"))';
                           (exec-path-from-shell-initialie))))
;; Stop littering
(use-package no-littering
  :demand
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  )

;; server mode
(use-package emacs
  :init
  (unless (and (fboundp 'server-running-p) (server-running-p))
    (server-start)))

;; pair parens
(use-package emacs
  :hook
  ((org-jupyter-mode . (lambda () (rc/add-local-electric-pairs '())))
                                   (org-mode . (lambda () (rc/add-local-electric-pairs '((?~ . ?~))))))
  :init
  (electric-pair-mode +1)
  (setq electric-pair-preserve-balance nil)
  ;; Dont skip newline when auto-pairing
  (setq electric-pair-skip-whitespace-chars '(9 32))
  ;; Mode specific local electric pairs
  (defconst rc/default-electric-pairs electric-pair-pairs)
  (defun rc/add-local-electric-pairs (pairs)
    "Example usage:
(add-hook 'jupyter-org-interaction-mode '(lambda() (set-local-electric-pairs '())))
"
    (setq-local electric-pair-pairs (append rc/default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; disable auto pairing for < >
  (add-function :before-until electric-pair-inhibit-predicate (lambda (c) (eq c ?< )))
  )

;; Rename file
(use-package emacs
  :init
  (defun rc/rename-current-file ()
    "rename the current visiting file and switch buffer focus to it."
    (interactive)
    (let ((new-filename (rc/expand-filename-props
                         (format "Rename %s to: " (file-name-non-directory
                                                   (buffer-file-name))))))
      (if (null (file-writable-p new-filename))
          (user-error "New file not writable: %s" new-filename))
      (rename-file (buffer-file-name) new-filename 1)
      (find-alternative-file new-filename)
      (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename))))
  (defun rc/expand-filename-prompt (prompt)
    "return expanded filename prompt."
    (exapnd-file-name (read-file-name prompt)))
  )

;; xref
(use-package xref
  :straight (:type built-in)
  :init
  (setq xref-prompt-for-identifier nil)
  )

;; Dont close windows on escape
(use-package emacs
  :init
  (defadvice keyboard-escape-quit
      (around keyboard-escape-quit-dont-close-windows activate
             )
    (let ((buffer-quit-function (lambda () () )))
      ad-do-it))
  )

(provide 'init-core)

;;; init-core.el ends here. o