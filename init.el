(require 'package)
;; Add MELPA to `list-packages'.
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Easy binding for switching between windows
(global-set-key (kbd "M-o") 'other-window)

;; IDO mode everywhere
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;; Start server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package hydra)
(use-package org-fc
	:load-path "~/Dev/org-fc"
  :custom (org-fc-directories '("~/org/flashcards"))
  :config
  (require 'org-fc-hydra))
(global-set-key (kbd "C-c f") #'org-fc-type-normal-init)
(global-set-key (kbd "C-c r") #'org-fc-review-all)

;; Set tab widths
(setq-default tab-width 2)

;; Display options
(use-package ef-themes
  :init
  (load-theme 'ef-summer t)
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

(setq gdscript-godot-executable "/usr/local/bin/godot")

;; Create jekyll post
(setq blog-home "~/Dev/Websites/cidney.org")

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


;; Elfeed
(setq elfeed-feeds
      '(
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
        ("https://zenhabits.net/feed/" productivity)
        )
      )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(chart-face-color-list
	 '("#ef7969" "#2fe029" "#ffcf00" "#7f90ff" "#e07fff" "#70d3f0" "#ffaab4" "#75ef30" "#f9ff00" "#9fc6ff" "#fad0ff" "#afefff"))
 '(custom-safe-themes
	 '("e5a748cbefd483b74b183d7da4fca6228207a6bf9be9792dc85403a186724e1f" "aee4c6b492ad130f13868464e4d7f2b2846de9b7f0d2933499c907f47dc010f4" "41bbaed6a17405ee6929c7e1f8035cffd05d0ebf3f08ce388da0e92c63fb6cef" default))
 '(elfeed-feeds
	 '("http://www.nathalielawhead.com/candybox/feed"
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
		 ("https://zenhabits.net/feed/" productivity)) t)
 '(flymake-error-bitmap '(flymake-double-exclamation-mark ef-themes-mark-delete))
 '(flymake-note-bitmap '(exclamation-mark ef-themes-mark-select))
 '(flymake-warning-bitmap '(exclamation-mark ef-themes-mark-other))
 '(ibuffer-deletion-face 'ef-themes-mark-delete)
 '(ibuffer-filter-group-name-face 'bold)
 '(ibuffer-marked-face 'ef-themes-mark-select)
 '(ibuffer-title-face 'default)
 '(ispell-dictionary nil)
 '(package-selected-packages
	 '(gdscript-mode eglot yasnippet use-package solarized-theme org-yaap org-drill modus-themes mastodon magit jsonrpc ink-mode hydra graphviz-dot-mode flymake external-completion emms ement elfeed-tube ef-themes discover dictionary darkroom csharp-mode company babel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
