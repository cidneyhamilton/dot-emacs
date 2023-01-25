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

;; EWW setings
(setq browse-url-browser-function 'eww-browse-url)

;; Start server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Helm
(use-package helm
  :straight t)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)

(add-to-list 'auto-mode-alist '("\\.html.erb" . html-erb-mode))

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

;; Godot
(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

(setq gdscript-godot-executable "~/Dev/Godot/Godot_v3.5.1-stable_x11.64")

;; LSP mode fix
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

;; Org mode setup
(require 'org)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-capture-templates
      '(("d" "Distraction" entry (file+headline "~/org/distractions.org" "Distractions") "* %?\n%T")
        ("i" "Inbox" entry (file+headline "~/org/inbox.org" "Inbox") "* %?\n%T")))

(setq org-todo-keywords
      '((sequence "TODO" "|" "WAITING" "DONE")))
(setq org-agenda-include-diary t)                                               
(setq org-agenda-include-all-todo t)
(setq org-default-notes-file "~/org/inbox.org")
(setq org-src-preserve-indentation nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)))

;; LSP
(use-package lsp-mode
  :ensure t
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-keymap-prefix "C-c l"))

;; (require 'dap-unity)

;; Mastodon
(setq mastodon-instance-url "https://social.city-of-glass.net"
      mastodon-active-user "cidney")

;; CSHARP
(use-package csharp-mode
  :straight t
  :defer t
  :init
  (my/hook "csharp-mode" csharp-mode-hook
    (my/set-fill-column 100)
    (c-toggle-auto-newline)
    (gsetq-local c-basic-offset 4)))

;; Elfeed
(setq elfeed-feeds
      '(
        ("https://www.reddit.com/r/writing.rss" writing reddit)        
        ("https://www.reddit.com/r/emacs.rss" emacs reddit)
        ("https://www.reddit.com/r/German.rss" german reddit)
        ("https://www.reddit.com/r/gamedev.rss" gamedev reddit)
        ("https://erzadel.net/feed.xml" people)
        ("https://rusingh.com/feed/" people webdev)
        ("https://christine.website/blog.rss" webdev)
        ("https://deadsuperhero.com/rss/" people)
        ("https://nolanlawson.com/feed" webdev)
        ("https://www.brainonfire.net/blog/" people webdev)
        ("http://cidney.org/feed.xml" people)
        ("https://www.brainonfire.net/blog/posts.atom" people webdev)
        ("https://babbagefiles.xyz/posts/index.xml" people webdev)
        ("https://blogghoran.se/feed" people)
        ("https://passionandsoul.com/blog/feed" people)
        ("https://theundercoverintrovert.com/feed" writing)
        ("https://cygnusentertainment.com/blog/feed" gamedev)
        ("https://amandapalmer.net/posts/feed" people)
        ("https://www.neilgaiman.com/feed/journal/" people writing)
        ("https://blindjournalist.wordpress.com/rss" people writing)
        ("https://jekyllrb.com/feed.xml" webdev)
        ("https://www.inklestudios.com/blog/" gamedev)
        ("https://laurakalbag.com/posts/index.xml" webdev)
        ("https://ar.al/index.xml" webddev)
        ("https://emshort.blog/feed" gamedev)
        ("https://ben304.blogspot.com/feeds/posts/default?alt=rss" gamedev)
        ("https://www.gibberlings3.net/rss/1-infinity-engine-modding-news.xml" gamedev)
        ("https://www.baldurbjarnason.com/feed.xml" webdev)
        ("https://drewdevault.com/blog/index.xml" webdev)
        ("http://jeffmachwrites.com/rss" writing people)
        ("https://godotengine.org/rss.xml" gamdev)
        ("https://victoriacorva.xyz/feed" writing)
        ("http://decafbad.net/feed/index.xml" people)
        ("https://alexschroeder.ch/wiki/feed/full" people emacs)
        ("https://pluralistic.net/rss" news)
        ("https://craphound.com/feed" news)
        ("http://pedestrianobservations.com/feed" people)
        ("https://sachachua.com/blog/feed" emacs)
        ("https://abagond.wordpress.com/feed" people)
        )
      )

;; Unity
(use-package unity
  :straight (unity :type git :host github :protocol ssh
                   :repo "elizagamedev/unity.el"
                   :files ("*.el" "*.c"))
  :config
  (unity-build-code-shim)
  (unity-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(ispell-dictionary nil)
 '(org-agenda-files '("~/org/projects.org"))
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
