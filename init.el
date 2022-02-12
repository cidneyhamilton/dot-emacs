(require 'package)

;; Add package sources
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)

;; Start server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; ensure use-package is present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

;; No need for backups
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq make-backuop-files nil)

;; tab-width
(setq-default tab-width 1)

;; Display options
(use-package solarized-theme
  :init
  (load-theme 'solarized-zenburn t)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

;; Magit keybindings
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

;; Load mail configuration
(load-file "~/.emacs.d/mail.el")

;; Load IRC configuration
(setq rcirc-default-nick "cidney")
(setq erc-server "irc.libera.chat"
      erc-nick "cidney"
      erc-user-full-name "Cidney Hamilton"
      erc-autojoin-channels-alist '(("irc.libera.chat" "#emacsconf" "#indieweb" "#emacs"))
      )
      
;; Unity and CSHARP configuration
(setenv "FrameworkPathOverride" "/lib/mono/4.5")

;; Org mode setup
(require 'org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
(setq org-agenda-include-diary t)                                               
(setq org-agenda-include-all-todo t)
(setq org-default-notes-file "~/org/inbox.org")

;; CSHARP
(use-package csharp-mode
  :ensure t
  :init
  (defun my/csharp-mode-hook ()
    (setq-local lsp-auto-guess-root t)
    (lsp))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook))

;; Unity
(load-file "~/.emacs.d/unity.el")
(add-hook 'after-init-hook #'unity-build-code-shim)
(add-hook 'after-init-hook #'unity-setup)

;; Music
(load-file "~/.emacs.d/emms.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
			'("0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(elfeed-feeds
			'("https://anchor.fm/s/52f2db0c/podcast/rss" "https://nolanlawson.com/feed" "https://nolanlawson.com/" "https://www.brainonfire.net/blog/" "http://cidney.org/feed.xml" "https://www.brainonfire.net/blog/posts.atom" "https://babbagefiles.xyz/posts/index.xml" "https://whatthefuckjusthappenedtoday.com/rss.xml" "https://heathermeeker.com/rss" "https://samkriss.com/rss" "https://blogghoran.se/feed" "https://itsfoss.com/feed/" "passionandsoul.com/blog/feed" "https://theundercoverintrovert.com/feed" "cygnusentertainment.com/blog/feed" "https://amandapalmer.net/posts/feed" "https://www.neilgaiman.com/feed/journal/" "https://blindjournalist.wordpress.com/rss" "https://jekyllrb.com/feed.xml" "https://www.inklestudios.com/blog/" "https://laurakalbag.com/posts/index.xml" "https://ar.al/index.xml" "https://www.youtube.com/feeds/videos.xml?channel_id=UCnPM1kSZf91ZGkcgy95Q" "https://kensgame.com/blog/feed" "https://emshort.blog/feed" "https://ben304.blogspot.com/feeds/posts/default?alt=rss" "https://blog.unity.com/technology/speed-up-your-programmer-workflows" "https://blog.unity.com/feed" "https://www.gibberlings3.net/rss/1-infinity-engine-modding-news.xml/" "https://www.baldurbjarnason.com/feed.xml" "https://drewdevault.com/blog/index.xml" "http://jeffmachwrites.com/rss" "https://godotengine.org/rss.xml" "https://blog.unity.com/rss-feeds" "https://victoriacorva.xyz/feed" "http://decafbad.net/feed/index.xml" "https://alexschroeder.ch/wiki/feed/full" "https://pluralistic.net/rss" "https://craphound.com/feed" "http://pedestrianobservations.com/feed" "https://sachachua.com/blog/feed" "https://abagond.wordpress.com/feed"))
 '(org-agenda-files '("~/org/gtd.org"))
 '(org-log-into-drawer t)
 '(package-selected-packages
			'(quelpa-use-package quelpa jabber helm bitlbee org-jira mpv which-key omnisharp emms emacsql emacsql-sqlite lsp-mode magit markdown-mode elfeed csharp-mode solarized-theme elpher darkroom ink-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
