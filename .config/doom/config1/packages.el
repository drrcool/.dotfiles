;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;
(package! apheleia)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! beacon)
(package! tldr)
(package! rainbow-mode)
(package! org-auto-tangle)
(package! fzf :pin "21912ebc7e1084aa88c9d8b7715e782a3978ed23")


(package! beacon)
(package! super-save
  :disable t
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")

(package! vundo
  :recipe (:host github
           :repo "casouri/vundo")
  :pin "16a09774ddfbd120d625cdd35fcf480e76e278bb")
(package! modus-themes :pin "ee35a9af344d2b2920589ec4d66e9cijjdsfasf")
(package! ef-themes :pin "3f9628750f8ff544169d4924e8c51f49b31f39e1")
(package! svg-tag-mode :pin "efd22edf650fb25e665269ba9fed7ccad0771a2f")
(package! focus :pin "9dd85fc474bbc1ebf22c287752c960394fcd465a")
(package! good-scroll
  :disable EMACS29+
  :pin "a7ffd5c0e5935cebd545a0570f64949077f71ee3")
(package! aggressive-indent :pin "70b3f0add29faff41e480e82930a231d88ee9ca7")
(package! cov :pin "cd3e1995c596cc227124db9537792d8329ffb696")

(package! grammarly
  :recipe (:host github
           :repo "emacs-grammarly/grammarly")
  :pin "e47b370faace9ca081db0b87ae3bcfd73212c56d")

(package! eglot-grammarly
  :disable (not (modulep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-grammarly/eglot-grammarly")
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")

(package! flycheck-grammalecte
  :recipe (:host github
           :repo "milouse/flycheck-grammalecte")
  :pin "314de13247710410f11d060a214ac4f400c02a71")

(package! flycheck-posframe)

(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")


(package! elpy :pin "ae7919d94659eb26d4146d4c3422c5f4c3610837")
(package! semgrep
  :disable t
  :recipe (:host github
           :repo "Ruin0x11/semgrep.el")
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")
(package! magit-pretty-graph
  :recipe (:host github
           :repo "georgek/magit-pretty-graph")
  :pin "26dc5535a20efe781b172bac73f14a5ebe13efa9")

(package! evil-escape :disable t)

(unpin! lsp-mode)
(unpin! dap-mode)
(package! org-wild-notifier)
(package! org-modern)
(package! moe-theme)
(package! evil-god-state)
(package! rainbow-mode)
(package! evil-dvorak)
