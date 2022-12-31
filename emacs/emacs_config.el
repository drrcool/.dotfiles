(use-package coverlay
:ensure t
:straight '(coverlay :type git
			   :host github :repo "twada/coverlay.el"))

(use-package css-in-js-mode
  
	:ensure t :straight '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))
      (use-package origami :ensure t
	:straight '(origamd
		    :type git
		    :host github
		    :repo "gregsexton/origami.el"))

(use-package corfu
  :ensure t
  :straight (corfu :host github :repo "minad/corfu")
 :custom
 (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;; (corfu-auto t)                 ;; Enable auto completion
 (corfu-separator ?\s)          ;; Orderless field separator
;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
 (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
 (corfu-preview-current t)    ;; Disable current candidate preview
 (corfu-preselect 'prompt)      ;; Preselect the prompt
;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;; (corfu-scroll-margin 5)        ;; Use scroll margin

;; Enable Corfu only for certain modes.
 :hook ((prog-mode . corfu-mode)
	(shell-mode . corfu-mode)
	(eshell-mode . corfu-mode))

;; Recommended: Enable Corfu globally.
;; This is recommended since Dabbrev can be used globally (M-/).
;; See also `corfu-excluded-modes'.
:init
(global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))
