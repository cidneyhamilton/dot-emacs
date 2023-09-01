(require 'package)
;; Add MELPA to `list-packages'.
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Add Unity.el to load path
(add-to-list 'load-path "~/Dev/unity.el/")
;; Ignore Unity meta files
(add-to-list 'completion-ignored-extensions ".meta")
; Unity and CSHARP configuration
(when (eq system-type 'gnu/linux)
  (setenv "FrameworkPathOverride" "/lib/mono/4.5"))

;; Start server
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

(setq ef-themes-to-toggle '(ef-day ef-night))

;; Magit keybindings
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

;; Load mail configuration
(load-file "~/.emacs.d/mail.el")

;; Load IRC configuration
;; (setq rcirc-default-nick "cidney")
;; (setq erc-server "irc.libera.chat"
;;       erc-nick "cidney"
;;       erc-user-full-name "Cidney Hamilton"
;;       erc-autojoin-channels-alist '(("irc.libera.chat" "#emacsconf" "#indieweb" "#emacs"))
;;       )

;; GDScript
(require 'gdscript-mode)
(setq gdscript-godot-executable "/usr/local/bin/godot")
(setq gdscript-gdformat-save-and-format t)

;; Org mode setup
(require 'org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-src-tabs-act-natively t)
(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "~/org/projects.org" "Inbox") "* %?\n%T")))
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

;; TODO list
(setq org-todo-keywords
      '((sequence "TODO" "|" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-default-notes-file "~/org/inbox.org")
(setq org-src-preserve-indentation nil)

;; Mastodon
(use-package mastodon
   :ensure t)
(setq mastodon-instance-url "https://social.city-of-glass.net"
      mastodon-active-user "cidney")

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
 '(ansi-color-names-vector
	 ["#184956" "#fa5750" "#75b938" "#dbb32d" "#4695f7" "#f275be" "#41c7b9" "#103c48"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#41c7b9")
 '(cua-normal-cursor-color "#adbcbc")
 '(cua-overwrite-cursor-color "#dbb32d")
 '(cua-read-only-cursor-color "#75b938")
 '(custom-safe-themes
	 '("cca1d386d4a3f645c2f8c49266e3eb9ee14cf69939141e3deb9dfd50ccaada79" "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5" "e5a748cbefd483b74b183d7da4fca6228207a6bf9be9792dc85403a186724e1f" "032426ec19e515fd3a54b38016a1c5e4ec066be3230198cb3df82d05630a02ed" "c06aa0ddb649e4e45f36dd95de98263672864074373937e65a23c8338f52c6af" "2141b59c9b098b476a7e20f7a621985b5d89544ae22a8d4b79b574f1203b6496" "49887e6f0c666dfc10fad4c23c7a83a176cb296968648c02b85deec25bb11103" "bcfeecf5f2ee0bbc64450f7c5155145d8d2c590b1310a898c505f48b4b5f4c75" "02790c735d32ad3b28c630329fdfc503ea62077d088b0c52302ab61e5a3b037e" "41bbaed6a17405ee6929c7e1f8035cffd05d0ebf3f08ce388da0e92c63fb6cef" "c6b317b294f9e0ecf7290a6d76b4c96ffd52213cdcb3fdad5db29141c63866cf" "20d3ce5f5cb95716edca608ef7bbc27d9f8d66c9a51200f7be3f08c107810f3e" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(elfeed-feeds
	 '("https://fediversereport.com/feed/" "http://feeds.feedblitz.com/daedtech/www" "https://jessmahler.com/feed/" "https://buttondown.email/jackyalcine/rss"
		 ("https://abagond.wordpress.com/feed" people)
		 ("https://alexschroeder.ch/wiki/feed/full" people emacs)
		 ("https://amandapalmer.net/posts/feed" people)
		 ("https://ar.al/index.xml" webddev)
		 ("https://babbagefiles.xyz/posts/index.xml" people webdev)
		 ("https://baldurbjarnason.com/feed.xml" webdev)
		 ("https://ben304.blogspot.com/feeds/posts/default?alt=rss" gamedev)
		 ("https://blindjournalist.wordpress.com/rss" people writing)
		 ("http://blog.zarfhome.com/feeds/posts/default" people gamedev)
		 ("https://blogghoran.se/feed" people fediverse)
		 ("https://brainonfire.net/blog/posts.atom" people webdev)
		 ("https://www.cggpodcast.com/feed.xml" gamedev)
		 ("https://christine.website/blog.rss" webdev)
		 ("https://cidney.org/feed.xml" people)
		 ("https://craphound.com/feed" news)
		 ("https://cygnusentertainment.com/blog/feed" gamedev)
		 ("https://deadsuperhero.com/rss/" people gamedev)
		 ("https://decafbad.net/feed/index.xml" people)
		 ("https://drewdevault.com/blog/index.xml" webdev)
		 ("https://eli.li/feed.rss" people)
		 ("https://emshort.blog/feed" gamedev)
		 ("https://erzadel.net/feed.xml" people fediverse)
		 ("https://ethanmarcotte.com/wrote/feed.xml" people)
		 ("https://gibberlings3.net/rss/1-infinity-engine-modding-news.xml" gamedev)
		 ("https://inklestudios.com/blog/" gamedev)
		 ("https://jeffmachwrites.com/rss" writing people)
		 ("https://jekyllrb.com/feed.xml" webdev)
		 ("https://laurakalbag.com/posts/index.xml" webdev)
		 ("http://ljwrites.blog/index.xml")
		 ("https://neilgaiman.com/feed/journal/" people writing)
		 ("https://nolanlawson.com/feed" people webdev)
		 ("https://passionandsoul.com/blog/feed" people)
		 ("https://pcwrede.com/pcw-wp/feed/" writing people)
		 ("https://pedestrianobservations.com/feed" people)
		 ("https://pluralistic.net/rss" news)
		 ("https://protesilaos.com/master.xml" people)
		 ("https://rosenzweig.io/feed.xml")
		 ("https://rusingh.com/feed/" people webdev fediverse)
		 ("https://sachachua.com/blog/feed" emacs)
		 ("https://theundercoverintrovert.com/feed" writing people)
		 ("https://victoriacorva.xyz/feed" writing)
		 ("https://wedistribute.org/feed" webdev)
		 ("https://xkcd.com/atom.xml")
		 ("https://zenhabits.net/feed/" personal)) t)
 '(fci-rule-color "#184956")
 '(highlight-changes-colors '("#f275be" "#af88eb"))
 '(highlight-symbol-colors
	 '("#4e3b57c24752" "#1c5d5c5162eb" "#58ac47cc4aec" "#3add4f876dec" "#316958f94870" "#53c94f1e4a56" "#1e6c515d7099"))
 '(highlight-symbol-foreground-color "#cad8d9")
 '(highlight-tail-colors
	 '(("#184956" . 0)
		 ("#489615" . 20)
		 ("#00a195" . 30)
		 ("#0068bb" . 50)
		 ("#ac8a0c" . 60)
		 ("#b45b24" . 70)
		 ("#b3478d" . 85)
		 ("#184956" . 100)))
 '(hl-bg-colors
	 '("#ac8a0c" "#b45b24" "#b42e2a" "#b3478d" "#7255b7" "#0068bb" "#00a195" "#489615"))
 '(hl-fg-colors
	 '("#103c48" "#103c48" "#103c48" "#103c48" "#103c48" "#103c48" "#103c48" "#103c48"))
 '(hl-paren-colors '("#41c7b9" "#dbb32d" "#4695f7" "#af88eb" "#75b938"))
 '(ispell-dictionary nil)
 '(lsp-ui-doc-border "#cad8d9")
 '(nrepl-message-colors
	 '("#fa5750" "#ed8649" "#dbb32d" "#489615" "#9fe35b" "#0068bb" "#41c7b9" "#f275be" "#af88eb"))
 '(org-agenda-files '("~/org/projects.org" "~/org/artists-way.org"))
 '(org-log-into-drawer t)
 '(package-enable-at-startup nil)
 '(package-selected-packages
	 '(helm modus-themes dictionary mastodon elfeed-tube yasnippet eglot ef-themes magit csharp-mode gdscript-mode emms markdown-mode elfeed solarized-theme elpher darkroom ink-mode))
 '(pos-tip-background-color "#184956")
 '(pos-tip-foreground-color "#cad8d9")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#75b938" "#184956" 0.2))
 '(term-default-bg-color "#103c48")
 '(term-default-fg-color "#adbcbc")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
	 '((20 . "#fa5750")
		 (40 . "#edc78b22427e")
		 (60 . "#e582a02c3945")
		 (80 . "#dbb32d")
		 (100 . "#bd04b6d430ef")
		 (120 . "#acc8b7f232c9")
		 (140 . "#9bbab8ca349e")
		 (160 . "#8980b961366e")
		 (180 . "#75b938")
		 (200 . "#6f7bbe1e677d")
		 (220 . "#69aac06b7c8c")
		 (240 . "#6126c2cb9118")
		 (260 . "#54b0c53ea56e")
		 (280 . "#41c7b9")
		 (300 . "#4cb7b3e4d313")
		 (320 . "#4d97a9dedf7a")
		 (340 . "#4b8c9fc5ebc3")
		 (360 . "#4695f7")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 '(unspecified "#103c48" "#184956" "#b42e2a" "#fa5750" "#489615" "#75b938" "#ac8a0c" "#dbb32d" "#0068bb" "#4695f7" "#b3478d" "#f275be" "#00a195" "#41c7b9" "#adbcbc" "#103c48"))
 '(xterm-color-names
	 ["#184956" "#fa5750" "#75b938" "#dbb32d" "#4695f7" "#f275be" "#41c7b9" "#ece3cc"])
 '(xterm-color-names-bright
	 ["#103c48" "#ed8649" "#72898f" "#103c48" "#adbcbc" "#af88eb" "#cad8d9" "#fbf3db"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
