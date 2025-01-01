(require 'package)

(add-to-list 'package-archives'("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package) (package-refresh-contents) (package-install 'use-package))

(setq mastodon-instance-url "http://social.cidney.org"
      mastodon-active-user "cidney")

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(add-to-list 'load-path "~/.emacs.d/unity.el/")
;; (add-to-list 'load-path "~/.emacs.d/nov.el/")

(global-set-key (kbd "M-o") 'other-window)

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(setq package-check-signature nil)

(setenv "FrameworkPathOverride" "/lib/mono/4.5")

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Set tab widths
(setq-default tab-width 2)

;; Display options
(use-package ef-themes
  :init
  (load-theme 'ef-night t)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

(setq ef-themes-to-toggle '(ef-summer ef-night))

;; Magit keybindings
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

;; Load mail configuration
(load-file "~/.emacs.d/mail.el")

;; GDScript
(use-package gdscript-mode
       :hook (gdscript-mode . eglot-ensure)
       )

(setq gdscript-godot-executable "/usr/bin/godot")

;; Create jekyll post
(setq blog-home "~/src/websites/cidney.org")

(defun web-draft
  (title)
  "Creates a new buffer and file for a blog post"
  (interactive "sTitle of blog post: ")
  (let
    ((filename
       (concat
         (format-time-string "%Y-%m-%d-%H:%M-")
         (replace-regexp-in-string " " "-"
           (downcase
             (replace-regexp-in-string "[^0-9a-zA-Z ]" "" title))))))
    (switch-to-buffer
      (generate-new-buffer filename))
    (insert
      (concat
        (mapconcat 'identity
                                                                        '("---" "layout: post")
                                                                        "\n")
        "\n" "title: '" title "'\n" "date: '"
        (format-time-string "%Y-%m-%d-%H:%M:%S %z")
        "'\n" "---\n"))
    (write-file
     (concat blog-home "/_posts/" filename ".md"))))
 

;; Org mode setup
(require 'org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-src-tabs-act-natively t)
(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "~/org/projects.org" "Inbox") "* %?\n%T")))
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; TODO list
(setq org-todo-keywords
      '((sequence "TODO" "|" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-default-notes-file "~/org/inbox.org")
(setq org-src-preserve-indentation nil)

;; Use EWW as default browser within Emacs
(setq browse-url-browser-function 'eww-browse-url)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(chart-face-color-list
   '("#ef7969" "#2fe029" "#ffcf00" "#7f90ff" "#e07fff" "#70d3f0" "#ffaab4" "#75ef30" "#f9ff00" "#9fc6ff" "#fad0ff" "#afefff"))
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("8093e0f40c724d32955ae65b7122ff74ce6aa9a86e408712e8dbfb0e325a3ad7" "f019002925408f081e767c515e4fb4b1d7f1462228d6cd32ff66f06a43671527" "0a953c81f5798aa99cafbc4aa8a56d16827442400028f6c1eab0c43061ea331c" "d0dc7861b33d68caa92287d39cf8e8d9bc3764ec9c76bdb8072e87d90546c8a3" "b93039071f490613499b76c237c2624ae67a9aafbc717da9b4d81f456344e56e" "9fba87dbc0f14d5650006893ed53088be71f16d57b749394d9c485ef2326e85f" "01cad03be8c042a9941fda5a484280629ee2cc83fe084af6d19376c83141c91b" "a087e01778a85f8381b2aa2b7b0832951aea078621b38844b6c8c8d638d73e3b" "97283a649cf1ffd7be84dde08b45a41faa2a77c34a4832d3884c7f7bba53f3f5" "4f03e70554a58349740973c69e73aefd8ce761a77b22a9dc52a19e708532084a" "fb7595c9571f2bd41635745d12551f35322296b70330056ddd0020ab2374671c" "587ce9a1a961792114991fd488ef9c3fc37f165f6fea8b89d155640e81d165a3" "5e41864cbdd81b18d1fa62f09971a55a121a939238ca4c66faafcfcafb976c3e" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "e5a748cbefd483b74b183d7da4fca6228207a6bf9be9792dc85403a186724e1f" "aee4c6b492ad130f13868464e4d7f2b2846de9b7f0d2933499c907f47dc010f4" "41bbaed6a17405ee6929c7e1f8035cffd05d0ebf3f08ce388da0e92c63fb6cef" default))
 '(elfeed-feeds
   '("https://virtualmoose.org/feed" "elfeed-add-feed" "https://blog.falkoloeffler.de/feed/" "https://ljwrites.blog/index.xml" "https://kechpaja.com/blog/feed/feed.xml" "https://kechpaja.com/blog/" "https://terikanefield.com/feed" "https://bildung.social/@computerspielemuseum.rss" "https://blindjournalist.wordpress.com/rss" "https://dosgame.club/@summerb" "https://ruby.social/@natbudin.rss" "http://social.wedistribute.org/@deadsuperhero.rss" "https://mastodon.gamedev.place/@wadjeteyegames.rss" "https://social.coop/@dynamic.rss" "https://lief3d.dev/" "https://lief3d.dev/feed" "http://www.nathalielawhead.com/candybox/feed"
     ("https://abagond.wordpress.com/feed" social-science)
     ("https://alexschroeder.ch/wiki/feed/full" emacs ttrpgs)
     ("https://ar.al/index.xml" webddev)
     ("https://babbagefiles.xyz/posts/index.xml" webdev)
     ("https://baldurbjarnason.com/feed.xml" webdev)
     ("https://ben304.blogspot.com/feeds/posts/default?alt=rss" gamedev art)
     ("https://blindjournalist.wordpress.com/rss" accessibility)
     ("https://blogghoran.se/feed" writing)
     ("https://brainonfire.net/blog/posts.atom" friends)
     ("https://craphound.com/feed" writing)
     ("https://cygnusentertainment.com/blog/feed" gamedev)
     ("https://deadsuperhero.com/rss/" friends)
     ("https://decafbad.net/feed/index.xml" productivity)
     ("https://emshort.blog/feed" gamedev interactive-fiction)
     ("https://gibberlings3.net/rss/1-infinity-engine-modding-news.xml" infinity-engine)
     ("https://inklestudios.com/blog/" gamedev interactive-fiction)
     ("https://jeffmachwrites.com/rss" writing)
     ("https://jekyllrb.com/feed.xml" webdev)
     ("https://laurakalbag.com/posts/index.xml" webdev)
     ("http://ljwrites.blog/index.xml" writing)
     ("https://neilgaiman.com/feed/journal/" writing)
     ("https://nolanlawson.com/feed" webdev)
     ("https://passionandsoul.com/blog/feed" pagn)
     ("https://pedestrianobservations.com/feed" transit)
     ("https://pluralistic.net/rss" econ)
     ("https://rusingh.com/feed/" webdev)
     ("https://sachachua.com/blog/feed" emacs)
     ("https://theundercoverintrovert.com/feed" writing)
     ("https://victoriacorva.xyz/feed" writing)
     ("https://zenhabits.net/feed/" productivity)))
 '(fci-rule-color "#073642")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark))
 '(flymake-note-bitmap '(exclamation-mark ef-themes-mark-select))
 '(flymake-warning-bitmap '(exclamation-mark))
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d7" "#07b9463d4d37" "#47a3341f358a" "#1d873c4056d5" "#2d87441c3362" "#43b7362e3199" "#061e418059d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(ibuffer-deletion-face 'ef-themes-mark-delete)
 '(ibuffer-filter-group-name-face 'bold)
 '(ibuffer-marked-face 'ef-themes-mark-select)
 '(ibuffer-title-face 'default)
 '(ispell-dictionary nil)
 '(lsp-ui-doc-border "#93a1a1")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files '("~/org/projects.org"))
 '(package-selected-packages
   '(tree-sitter tide web-mode lsp-tailwindcss typescript-mode skewer-mode nov lsp-mode elfeed ef-themes dash gdscript-mode eglot yasnippet use-package solarized-theme org-yaap org-drill modus-themes mastodon magit jsonrpc ink-mode hydra graphviz-dot-mode flymake external-completion emms ement elfeed-tube discover dictionary darkroom company babel))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4466ec20b5")
     (60 . "#c11679431550")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed992380000")
     (140 . "#96bf94d00000")
     (160 . "#8e5497440000")
     (180 . "#859900")
     (200 . "#77689bfc4636")
     (220 . "#6d449d475bfe")
     (240 . "#5fc09ea47093")
     (260 . "#4c69a01784aa")
     (280 . "#2aa198")
     (300 . "#303598e7affc")
     (320 . "#2fa1947dbb9b")
     (340 . "#2c889009c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
