;; (defun sheda/switch-to-mail-buffer-other-window ()
;;   "Jump to the Gnus' *Group* buffer."
;;   (interactive)
;;   (switch-to-buffer-other-window "*Group*"))

(defun sheda-communication/jabber-chat-with (jc jid &optional other-window)
  "Open an empty chat window for chatting with JID.
With a prefix argument, open buffer in other window.
Returns the chat buffer."
  (interactive (let* ((jid
                       (jabber-read-jid-completing "chat with:" nil t)) ; Calling with a non-nil require-match seems to fix the problem with Helm (i.e., no match except if you start with a backslash)
                      (account
                       (jabber-read-account nil jid)))
                 (list
                  account jid current-prefix-arg)))
  (let ((buffer (jabber-chat-create-buffer jc jid)))
    (if other-window
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun sheda-communication/jabber-connected ()
  "Tell if at least one jabber connection is established."
  (not (null jabber-connections)))

(defun sheda-communication/mu4e-me ()
  "Return the recipient (to:) corresponding to me."
  (cond ((string= system-name "azathoth.stephaner.labo.int") "stephane.rochoy")
        (t                                                   user-login-name)))

(defun sheda-communication/openpgp-signer ()
  "Return the fingerprint of the OpenPGP key corresponding to me."
  (cond ((string= system-name "azathoth.stephaner.labo.int") "2BD1A8B4F6C565A7CB10EE92EF889210135D89E8")   ;; Stéphane Rochoy
        (t                                                   "4B6CC1E70088B7010004F9976DA733C80B15560D"))) ;; Sheda

(defun sheda-communication/mu4e-base-maildir ()
  "Return the base maildir."
  (expand-file-name (concat "/" (cond ((string= system-name "azathoth.stephaner.labo.int") "stormshield")
                                      (t                                                   user-login-name)))
                    user-home-directory))

(defun sheda-communication/mu4e-archive-maildir ()
  "Return the current archive maildir."
  (format "%s/archive/%s" (sheda-communication/mu4e-base-maildir) (format-time-string "%Y")))

(defun sheda-communication/mu4e-filter-by-mail-prefix (prefix sequence)
  "Filter the given sender/recipient SEQUENCE to only keep the ones having their mail starts with the given PREFIX."
  (seq-filter (lambda (sr)
                (s-starts-with-p prefix (cdr sr)))
              sequence))

(defun sheda-communication/mu4e-refile-folder-func (msg)
  "Select the folder according to MSG's From: and To: headers."
  (let* ((recipients          (append (mu4e-message-field msg :to) (mu4e-message-field msg :cc)))
         (current-maildir     (mu4e-message-field msg :maildir))
         (archive-maildir     (sheda-communication/mu4e-archive-maildir))
         (irp-archive-maildir (concat archive-maildir "/irp")))
    (sheda-core/message (format "Current maildir: %s" current-maildir))
    (cond
     ;; IRP-related:
     ((not (null (sheda-communication/mu4e-filter-by-mail-prefix "dp"    recipients))) irp-archive-maildir)
     ((not (null (sheda-communication/mu4e-filter-by-mail-prefix "ce"    recipients))) irp-archive-maildir)
     ((not (null (sheda-communication/mu4e-filter-by-mail-prefix "chsct" recipients))) irp-archive-maildir)
     ;; If refiling from /archive:
     ;; ((s-starts-with-p (format "%s/archive/" (sheda-communication/mu4e-base-maildir)) current-maildir)
     ;;  (progn
     ;;    (sheda-core/message "Message already in /archive.")
     ;;    (mu4e-ask-maildir "Maildir: ")
     ;;    ))
     ;; Default:
     (t archive-maildir))))

(defun sheda-communication/jump-to-containing-maildir (msg)
  "Jump to the maildir containing MSG."
  (let ((maildir (mu4e-message-field msg :maildir)))
    (mu4e~headers-jump-to-maildir maildir)))

(defun sheda-communication/jump-to-containing-maildir (msg)
  "Jump to the maildir containing MSG."
  (let ((maildir (mu4e-message-field msg :maildir)))
    (mu4e~headers-jump-to-maildir maildir)))

(defun sheda-communication/add-mu4e-buffer-to-persp-and-switch ()
  (persp-add-buffer (current-buffer) (persp-get-by-name "@Mu4e"))
  (persp-switch "@Mu4e"))

(defun sheda-communication/setup-alert-style-for-mu4e ()
  "Define the libnotify-mail style and make it the default for mu4e-alert."
  (alert-define-style 'libnotify-mail
                        :title    "Notify about mails using libnotify"
                        :notifier
                        (lambda  (info)
                          "Call alert-libnotify-notify after having added a mail icon to INFO."
                          (alert-libnotify-notify (plist-put info
                                                             :icon "/usr/share/icons/Adwaita/32x32/emblems/emblem-mail.png"))))
  (mu4e-alert-set-default-style 'libnotify-mail))

(defun sheda-communication/react-to-jabber-disconnection (&optional connection)
  "React to a Jabber disconnection."
  (alert-libnotify-notify
   (list :title "jabber"
         :message "Disconnected."
         :icon "/usr/share/icons/Adwaita/32x32/status/user-busy.png")))

(defun sheda-communication/react-to-jabber-connection (connection)
  "React to a Jabber connection."
  (alert-libnotify-notify
   (list :title "jabber"
         :message "Connected and authenticated."
         :icon "/usr/share/icons/Adwaita/32x32/status/user-available.png")))

(defun sheda-communication/adjust-mu4e-main-mode-map ()
  (evilified-state-evilify mu4e-main-mode mu4e-main-mode-map
    (kbd "/") 'mu4e-headers-search
    (kbd "q") 'bury-buffer
    (kbd "Q") 'mu4e-quit
    (kbd "u") 'mu4e-update-mail-and-index))

(defun sheda-communication/adjust-mu4e-headers-mode-map ()
  (evilified-state-evilify mu4e-headers-mode mu4e-headers-mode-map
    (kbd "t") 'mu4e-headers-next
    (kbd "s") 'mu4e-headers-prev
    (kbd "j") 'mu4e-headers-mark-thread
    (kbd "r") 'mu4e-view-mark-for-refile
    (kbd "/") 'mu4e-headers-search-narrow
    (kbd "w") 'mu4e-headers-query-prev))

(defun sheda-communication/add-mu4e-custom-headers-markers ()
  (add-to-list 'mu4e-headers-custom-markers
               '("Unreads"
                 (lambda (msg unused)
                   (memq 'unread (mu4e-message-field msg :flags))))))

(defun sheda-communication/adjust-mu4e-view-mode-map ()
  (evilified-state-evilify mu4e-view-mode mu4e-view-mode-map
    (kbd "<backtab>") 'org-previous-link
    (kbd "TAB")       'org-next-link
    (kbd "RET")       'browse-url-at-point
    (kbd "t")         'evil-next-visual-line
    (kbd "s")         'evil-previous-visual-line
    (kbd "T")         'mu4e-view-headers-next
    (kbd "S")         'mu4e-view-headers-prev
    (kbd "v")         'mu4e-view-verify-msg-popup))

(defun sheda-communication/adjust-mu4e-compose-mode-map ()
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
    "a" 'mml-attach-file
    "c" 'message-send
    "d" 'message-dont-send
    "e" 'mml-secure-message-sign-encrypt
    "s" 'mml-secure-message-sign
    "q" 'kill-this-buffer)
  )

(defun sheda-communication/mu4e-log-drop-contact (contact)
  (let ((mail (plist-get contact :mail)))
    (sheda-core/message (format "mu4e-rewrite-function: %s -> drop" mail))
    nil))

(defun sheda-communication/mu4e-log-keep-contact (contact)
  (let ((mail (plist-get contact :mail)))
    (sheda-core/message (format "mu4e-rewrite-function: %s -> keep" mail))
    contact))

(defvar sheda-communication~mu4e-contacts (list))

(defun sheda-communication/mu4e-rewrite-function (contact)
  "Rewrite/Drop contacts."
  (let ((name (or (plist-get contact :name) ""))
        (mail (plist-get contact :mail)))
    (cond
     ;; Common Typo:
     ((string-suffix-p "@stomshield.eu" mail)     nil)
     ((string-suffix-p "@stormshield.e" mail)     nil)
     ;; Deprecated Domains:
     ((string-suffix-p "@arkoon-netasq.com" mail) nil)
     ((string-suffix-p "@netasq.com" mail)        nil)
     ((string-suffix-p "@netasq.old" mail)        nil)
     ;; Keep everything else.
     (t (progn
          (add-to-list 'sheda-communication~mu4e-contacts (format "%s\t%s\t" mail name))
          contact)))))

(defun sheda-communication/mu4e-contacts ()
  "Retrieves contacts from mu4e, not mu to benefit from mu4e-rewrite-function."
  sheda-communication~mu4e-contacts
  ;; (let* ((contacts (list)))
  ;;   (maphash (lambda (k v) (setq contacts (append (list k) contacts) )) mu4e~contacts)
  ;;   contacts)
  )

(defun sheda-communication/helm-mu4e-contacts ()
  "Search for contacts."
  (interactive)
  (helm :sources 'helm-source-mu4e-contacts
        :buffer "*helm mu4e contacts*"))
