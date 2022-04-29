(customize-set-variable 'package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'general)
(eval-and-compile
  (require 'general)
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-default #'general-setq-default)
  (defalias 'gsetq-local #'general-setq-local))

(straight-use-package 'use-package)

;; No need for backups
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq make-backuop-files nil)

;; Start server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))


(defmacro my/hook (package-name hook &rest body)
  "Add a new function hook with the given BODY to the given HOOK.

PACKAGE-NAME is a unique prefix given to each function hook name."
  (let ((fun-name (intern (concat "my/hook--"
                                  (symbol-name hook)
                                  "--"
                                  package-name))))
    `(progn
       (eval-and-compile (defun ,fun-name () ,@body))
       (add-hook ',hook #',fun-name))))
(setf (get 'my/hook 'lisp-indent-function) 2)

(defun my/align-whitespace (start end)
  "Align columns by whitespace from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun my/set-fill-column (value)
  "Set the fill column of the buffer and update `whitespace-mode' to match."
  (whitespace-mode -1)
  (gsetq-local fill-column value)
  (whitespace-mode 1))

;; tab-width
(setq-default tab-width 1)

(use-package whitespace
  :diminish whitespace-mode global-whitespace-mode
  :custom
  (whitespace-line-column nil)
  (whitespace-style '(face lines-tail)))


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

(use-package package
  :config
  ;; Add MELPA to `list-packages'.
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package flymake
  :hook
  (find-file . flymake-mode))

;; Important for Unity development
(gsetq-default buffer-file-coding-system 'utf-8-unix
               fill-column 80
               indent-tabs-mode nil
               require-final-newline t
               sentence-end-double-space nil
               tab-width 8)

(add-to-list 'completion-ignored-extensions ".meta")

;; Unity and CSHARP configuration
(when (eq system-type 'gnu/linux)
  (setenv "FrameworkPathOverride" "/lib/mono/4.5"))


;; Org mode setup
(require 'org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
(setq org-agenda-include-diary t)                                               
(setq org-agenda-include-all-todo t)
(setq org-default-notes-file "~/org/inbox.org")

;; LSP
(use-package lsp-mode
  :ensure t
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-keymap-prefix "C-c l"))

(require 'dap-unity)

;; CSHARP
(use-package csharp-mode
  :straight t
  :defer t
  :init
  (my/hook "csharp-mode" csharp-mode-hook
    (my/set-fill-column 100)
    (gsetq-local c-basic-offset 4)))

;; Unity
(use-package unity
  :straight (unity :type git :host github :protocol ssh
                   :repo "elizagamedev/unity.el"
                   :files ("*.el" "*.c"))
  :config
  ;; (unity-build-code-shim)
  (unity-setup))

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
   '("https://erzadel.net/feed.xml" "https://rusingh.com/feed/" "https://christine.website/blog.rss" "https://deadsuperhero.com/rss/" "https://anchor.fm/s/52f2db0c/podcast/rss" "https://nolanlawson.com/feed" "https://nolanlawson.com/" "https://www.brainonfire.net/blog/" "http://cidney.org/feed.xml" "https://www.brainonfire.net/blog/posts.atom" "https://babbagefiles.xyz/posts/index.xml" "https://whatthefuckjusthappenedtoday.com/rss.xml" "https://heathermeeker.com/rss" "https://samkriss.com/rss" "https://blogghoran.se/feed" "https://itsfoss.com/feed/" "passionandsoul.com/blog/feed" "https://theundercoverintrovert.com/feed" "cygnusentertainment.com/blog/feed" "https://amandapalmer.net/posts/feed" "https://www.neilgaiman.com/feed/journal/" "https://blindjournalist.wordpress.com/rss" "https://jekyllrb.com/feed.xml" "https://www.inklestudios.com/blog/" "https://laurakalbag.com/posts/index.xml" "https://ar.al/index.xml" "https://www.youtube.com/feeds/videos.xml?channel_id=UCnPM1kSZf91ZGkcgy95Q" "https://kensgame.com/blog/feed" "https://emshort.blog/feed" "https://ben304.blogspot.com/feeds/posts/default?alt=rss" "https://blog.unity.com/technology/speed-up-your-programmer-workflows" "https://blog.unity.com/feed" "https://www.gibberlings3.net/rss/1-infinity-engine-modding-news.xml/" "https://www.baldurbjarnason.com/feed.xml" "https://drewdevault.com/blog/index.xml" "http://jeffmachwrites.com/rss" "https://godotengine.org/rss.xml" "https://blog.unity.com/rss-feeds" "https://victoriacorva.xyz/feed" "http://decafbad.net/feed/index.xml" "https://alexschroeder.ch/wiki/feed/full" "https://pluralistic.net/rss" "https://craphound.com/feed" "http://pedestrianobservations.com/feed" "https://sachachua.com/blog/feed" "https://abagond.wordpress.com/feed"))
 '(org-agenda-files '("~/org/gtd.org"))
 '(org-log-into-drawer t)
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(jabber helm bitlbee org-jira mpv which-key omnisharp emms emacsql emacsql-sqlite lsp-mode magit markdown-mode elfeed csharp-mode solarized-theme elpher darkroom ink-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
