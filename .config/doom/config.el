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
