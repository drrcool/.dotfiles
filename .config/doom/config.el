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
