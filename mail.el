(use-package mu4e
		:ensure nil
		:config

;; Allow getting email with mbsync
(setq mu4e-get-mail-command  "mbsync -a")

(setq
 mu4e-headers-skip-duplicates t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t

 mu4e-maildir       "~/Maildir"   ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-sent-folder   "/Sent"
 mu4e-attachments-dir "/Downloads"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash")

;; Send mail configuration
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
						user-mail-address "cidney@cidneyhamilton.com"
						user-full-name    "Cidney Hamilton")
)
