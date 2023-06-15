(require 'package)
;; Add MELPA to `list-packages'.
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; No need for backups
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq make-backup-files nil)

;; EWW setings
(setq browse-url-browser-function 'eww-browse-url)

;; GDscript
(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))
;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
(advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)

;; Add Unity.el to load path
(add-to-list 'load-path "~/Dev/unity.el/")

;; Ink mode
;; Path to the Inklecate binary, used to playtest
;; and to check for errors
(setq ink-inklecate-path "/home/cidney/Dev/tools/inklecate")

;; Enable flymake (error reporting)
(add-hook 'ink-mode-hook 'flymake-mode)

;; Set indentation level
(add-hook 'ink-mode-hook (lambda () (setq tab-width 2)))

;; Start server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(add-to-list 'auto-mode-alist '("\\.html.erb" . html-erb-mode))

;; tab-width
(setq-default tab-width 2)

;; Display options
(use-package solarized-theme
  :init
  (load-theme 'solarized-light t)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

;; Magit keybindings
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

;; Load mail configuration
(load-file "~/.emacs.d/mail.el")

;; Google Translate
(setq google-translate-default-source-language "de")
(setq google-translate-default-target-language "en")
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

;; Load IRC configuration
(setq rcirc-default-nick "cidney")
(setq erc-server "irc.libera.chat"
      erc-nick "cidney"
      erc-user-full-name "Cidney Hamilton"
      erc-autojoin-channels-alist '(("irc.libera.chat" "#emacsconf" "#indieweb" "#emacs"))
      )

(use-package flymake
  :hook
  (find-file . flymake-mode))

(add-to-list 'completion-ignored-extensions ".meta")

;; Org mode setup
(require 'org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-src-tabs-act-natively t)
(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "~/org/projects.org" "Inbox") "* %?\n%T")))
(add-hook 'org-mode-hook 'turn-on-flyspell)

(setq org-todo-keywords '("TODO" "NEXT" "WAITING" "MAYBE" "DONE"))
(setq org-agenda-include-all-todo t)
(setq org-default-notes-file "~/org/inbox.org")
(setq org-src-preserve-indentation nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
	 (ruby . t)))

;; LSP
;; (use-package lsp-mode
;;   :ensure t
;;   :bind-keymap
;;   ("C-c l" . lsp-command-map)
;;   :custom
;;   (lsp-keymap-prefix "C-c l"))

;; (require 'dap-unity)

;; Mastodon
(setq mastodon-instance-url "https://social.city-of-glass.net"
      mastodon-active-user "cidney")


;; Elfeed
(setq elfeed-feeds
      '(
        ("https://erzadel.net/feed.xml" people fediverse)
        ("https://rusingh.com/feed/" people webdev fediverse)
        ("https://christine.website/blog.rss" webdev)
        ("https://deadsuperhero.com/rss/" people gamedev)
        ("https://nolanlawson.com/feed" people webdev)
        ("https://cidney.org/feed.xml" people)
        ("https://brainonfire.net/blog/posts.atom" people webdev)
        ("https://babbagefiles.xyz/posts/index.xml" people webdev)
        ("https://blogghoran.se/feed" people fediverse)
        ("https://passionandsoul.com/blog/feed" people)
        ("https://theundercoverintrovert.com/feed" writing people)
        ("https://cygnusentertainment.com/blog/feed" gamedev)
        ("https://amandapalmer.net/posts/feed" people)
        ("https://neilgaiman.com/feed/journal/" people writing)
        ("https://blindjournalist.wordpress.com/rss" people writing)
        ("https://jekyllrb.com/feed.xml" webdev)
        ("https://inklestudios.com/blog/" gamedev)
        ("https://laurakalbag.com/posts/index.xml" webdev)
        ("https://ar.al/index.xml" webddev)
        ("https://emshort.blog/feed" gamedev)
        ("https://ben304.blogspot.com/feeds/posts/default?alt=rss" gamedev)
        ("https://gibberlings3.net/rss/1-infinity-engine-modding-news.xml" gamedev)
        ("https://baldurbjarnason.com/feed.xml" webdev)
        ("https://drewdevault.com/blog/index.xml" webdev)
        ("https://jeffmachwrites.com/rss" writing people)
        ("https://victoriacorva.xyz/feed" writing)
        ("https://decafbad.net/feed/index.xml" people)
        ("https://alexschroeder.ch/wiki/feed/full" people emacs)
        ("https://pluralistic.net/rss" news)
        ("https://craphound.com/feed" news)
        ("https://pedestrianobservations.com/feed" people)
        ("https://sachachua.com/blog/feed" emacs)
        ("https://abagond.wordpress.com/feed" people)
        )
      )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(elfeed-feeds
	 '("http://ljwrites.blog/index.xml" "https://deadsuperhero.com/comments/feed/" "https://rosenzweig.io/feed.xml" "https://zenhabits.net/feed/"
		 ("https://erzadel.net/feed.xml" people fediverse)
		 ("https://rusingh.com/feed/" people webdev fediverse)
		 ("https://christine.website/blog.rss" webdev)
		 ("https://deadsuperhero.com/rss/" people gamedev)
		 ("https://nolanlawson.com/feed" people webdev)
		 ("https://cidney.org/feed.xml" people)
		 ("https://brainonfire.net/blog/posts.atom" people webdev)
		 ("https://babbagefiles.xyz/posts/index.xml" people webdev)
		 ("https://blogghoran.se/feed" people fediverse)
		 ("https://passionandsoul.com/blog/feed" people)
		 ("https://theundercoverintrovert.com/feed" writing people)
		 ("https://cygnusentertainment.com/blog/feed" gamedev)
		 ("https://amandapalmer.net/posts/feed" people)
		 ("https://neilgaiman.com/feed/journal/" people writing)
		 ("https://blindjournalist.wordpress.com/rss" people writing)
		 ("https://jekyllrb.com/feed.xml" webdev)
		 ("https://inklestudios.com/blog/" gamedev)
		 ("https://laurakalbag.com/posts/index.xml" webdev)
		 ("https://ar.al/index.xml" webddev)
		 ("https://emshort.blog/feed" gamedev)
		 ("https://ben304.blogspot.com/feeds/posts/default?alt=rss" gamedev)
		 ("https://gibberlings3.net/rss/1-infinity-engine-modding-news.xml" gamedev)
		 ("https://baldurbjarnason.com/feed.xml" webdev)
		 ("https://drewdevault.com/blog/index.xml" webdev)
		 ("https://jeffmachwrites.com/rss" writing people)
		 ("https://victoriacorva.xyz/feed" writing)
		 ("https://decafbad.net/feed/index.xml" people)
		 ("https://alexschroeder.ch/wiki/feed/full" people emacs)
		 ("https://pluralistic.net/rss" news)
		 ("https://craphound.com/feed" news)
		 ("https://pedestrianobservations.com/feed" people)
		 ("https://sachachua.com/blog/feed" emacs)
		 ("https://abagond.wordpress.com/feed" people)))
 '(ispell-dictionary nil)
 '(org-agenda-files '("~/org/projects.org" "~/org/artists-way.org"))
 '(org-log-into-drawer t)
 '(package-enable-at-startup nil)
 '(package-selected-packages
	 '(csharp-mode gdscript-mode emms markdown-mode elfeed solarized-theme elpher darkroom ink-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
