;;; packages.el --- sheda-communication layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Sheda
;;
;; Author: Sheda <sheda@fsfe.org>
;; URL: https://github.com/shedatc/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `sheda-communication-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sheda-communication/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sheda-communication/pre-init-PACKAGE' and/or
;;   `sheda-communication/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sheda-communication-packages
  '(
    helm-mu
    ;; jabber
    ;; (jabber-otr :excluded t)
    mu4e
    mu4e-alert
    ;; persp-mode
    )
)

;; (defun sheda-communication/init-helm-mu ()
;;   "Initialize the helm-mu package if the mu4e one is installed."
;;   (use-package helm-mu
;;     :if (configuration-layer/package-usedp 'mu4e)
;;     ;; :commands (list 'helm-mu 'helm-mu-contacts)
;;     :commands 'helm-mu
;;     :init
;;     (spacemacs/set-leader-keys
;;       "oc"  'sheda-communication/helm-mu4e-contacts
;;       ;; The "om" prefix is declared by sheda-communication/pre-init-mu4e.
;;       "omb" 'mu4e-headers-search-bookmark
;;       "omj" 'mu4e~headers-jump-to-maildir
;;       "omm" 'helm-mu)
;;     :config
;;     (advice-add 'mu4e~fill-contacts :before
;;                 (lambda (contact-data)
;;                   "Clear sheda-communication/mu4e-contacts first."
;;                   (setq sheda-communication~mu4e-contacts (list))))
;;     (defvar helm-source-mu4e-contacts
;;       (helm-build-in-buffer-source "Search mu4e contacts"
;;         :data #'sheda-communication/mu4e-contacts
;;         ;; :filtered-candidate-transformer #'helm-mu-contacts-transformer ;; XXX Need to adjust the format.
;;         :action '(("Compose email addressed to selected contacts." . helm-mu-compose-mail)
;;                   ("Get the emails from/to the selected contacts." . helm-mu-action-get-contact-emails)
;;                   ("Insert contacts at point."                     . helm-mu-action-insert-contacts)
;;                   ("Copy contacts to clipboard."                   . helm-mu-action-copy-contacts-to-clipboard))))
;;     (let* ((base (sheda-communication/mu4e-base-maildir)))
;;       (setq helm-mu-default-search-string (format "(m:%s/inbox OR m:%s/sent OR m:%s/irp) AND d:2w..now" base base base)))
;;     (helm-add-action-to-source "Jump to containing maildir" 'sheda-communication/jump-to-containing-maildir helm-source-mu)))

(defun sheda-communication/post-init-helm-mu ()
  "Configure the helm-mu package if the mu4e one is installed."
  (spacemacs/set-leader-keys
    "oc"  'sheda-communication/helm-mu4e-contacts
    ;; The "om" prefix is declared by sheda-communication/pre-init-mu4e.
    "omb" 'mu4e-headers-search-bookmark
    "omj" 'mu4e~headers-jump-to-maildir
    "omm" 'helm-mu)

  ;; XXX
  ;; (advice-add 'mu4e~fill-contacts :before
  ;;             (lambda (contact-data)
  ;;               "Clear sheda-communication/mu4e-contacts first."
  ;;               (setq sheda-communication~mu4e-contacts (list))))

  (defvar helm-source-mu4e-contacts
    (helm-build-in-buffer-source "Search mu4e contacts"
      :data #'sheda-communication/mu4e-contacts
      ;; :filtered-candidate-transformer #'helm-mu-contacts-transformer ;; XXX Need to adjust the format.
      :action '(("Compose email addressed to selected contacts." . helm-mu-compose-mail)
                ("Get the emails from/to the selected contacts." . helm-mu-action-get-contact-emails)
                ("Insert contacts at point."                     . helm-mu-action-insert-contacts)
                ("Copy contacts to clipboard."                   . helm-mu-action-copy-contacts-to-clipboard))))
  (let* ((base (sheda-communication/mu4e-base-maildir)))
    (setq helm-mu-default-search-string (format "(m:%s/inbox OR m:%s/sent OR m:%s/irp) AND d:2w..now" base base base)))
  ;; (helm-add-action-to-source "Jump to containing maildir" 'sheda-communication/jump-to-containing-maildir helm-source-mu)
  )

(defun sheda-communication/setup-jabber-hooks ()
  ;; jabber-alert-presence-hooks
  (add-hook 'jabber-alert-presence-hooks 'jabber-message-libnotify)

  ;; jabber-alert-message-hooks
  (add-hook 'jabber-alert-message-hooks 'jabber-message-libnotify)
  (add-hook 'jabber-alert-message-hooks 'jabber-message-display)
  (add-hook 'jabber-alert-message-hooks 'jabber-message-scroll)

  ;; jabber-post-connect-hooks
  (add-hook 'jabber-post-connect-hooks 'spacemacs/jabber-connect-hook)
  (add-hook 'jabber-post-connect-hooks 'jabber-keepalive-start)
  (add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)
  (add-hook 'jabber-post-connect-hooks 'sheda-communication/react-to-jabber-connection)

  ;; jabber-post-disconnect-hook (no 's')
  (add-hook 'jabber-post-disconnect-hook 'sheda-communication/react-to-jabber-disconnection)

  ;; jabber-lost-connection-hooks
  (add-hook 'jabber-lost-connection-hooks 'sheda-communication/react-to-jabber-disconnection)
  ;; (add-hook 'jabber-lost-connection-hooks 'jabber-connect-all) ;; XXX Try to reconnect but don't know if conflict with jabber-auto-reconnect.

  ;; jabber-alert-muc-hooks
  (add-hook 'jabber-alert-muc-hooks 'jabber-muc-libnotify)
  (add-hook 'jabber-alert-muc-hooks 'jabber-muc-display)
  (add-hook 'jabber-alert-muc-hooks 'jabber-muc-scroll)

  ;; jabber-chat-mode-hook
  (add-hook 'jabber-chat-mode-hook
            (lambda ()
              "Install some key bindings under the major mode leader key (,) when in chat mode."
              (spacemacs/set-leader-keys-for-major-mode 'jabber-chat-mode
                "l" 'jabber-chat-display-more-backlog))
            )
  )

(defun sheda-communication/post-init-jabber ()
  ;; Passwords taken from ~/.netrc.
  (setq jabber-account-list    '(("stephane.rochoy@frliljab01.one.local" (:connection-type . starttls))
                                 )
        jabber-history-enabled t
        jabber-history-dir     "~/.emacs.d/private/jabber-history"
        jabber-auto-reconnect  t
        ;; jabber-events-confirm-composing nil
        ;; jabber-chatstates-confirm       nil
        jabber-libnotify-icon "/usr/share/icons/Adwaita/32x32/status/user-available.png"
        )

  ;; DEBUG
  ;; (setq jabber-debug-keep-process-buffers t
  ;;       jabber-debug-log-xml              t
  ;;       )

  ;; XXX
  ;;
  ;; This variable is obsolete since 24.3;
  ;; use ‘display-buffer-alist’ instead.
  ;; This variable may be risky if used as a file-local variable.
  ;;
  ;; (setq special-display-regexps
  ;;       '(("jabber-chat"
  ;;          (width . 80)
  ;;          (scroll-bar-width . 16)
  ;;          (height . 15)
  ;;          (tool-bar-lines . 0)
  ;;          (menu-bar-lines 0)
  ;;          (font . "-GURSoutline-Courier New-normal-r-normal-normal-11-82-96-96-c-70-iso8859-1")
  ;;          (left . 80))))

  (sheda-communication/setup-jabber-hooks)

  (spacemacs/declare-prefix "oj" "jabber")
  (spacemacs/set-leader-keys
    "oc" 'sheda-communication/jabber-chat-with
    "oj" 'jabber-switch-to-roster-buffer)
  ;; (spacemacs/set-leader-keys-for-major-mode 'jabber-chat-mode
  ;;   (kbd "<ESC>") 'delete-window)
  )

(defun sheda-communication/init-jabber-otr ()
  (use-package jabber-otr
    :defer t
    :init
    (sheda-core/message "event: init: jabber-otr")))

(defun sheda-communication/pre-init-mu4e ()
  "Pre-initialize the mu4e package."

  (spacemacs/declare-prefix "om" "mu4e")

  ;; Decide on the base maildir (used to prefix all the queries and path) using
  ;; the hostname (system-name).
  ;; Also decide on my email address.
  (let* ((me   (sheda-communication/mu4e-me))
         (base (sheda-communication/mu4e-base-maildir))
         (in-inboxes  (format "( m:%s/inbox OR m:%s/irp )" base base)))
    (setq mu4e-maildir (expand-file-name ".mails" user-home-directory)
          mu4e-bookmarks
          (list (list (format "%s AND g:unread AND NOT g:trashed AND t:%s" in-inboxes me) "My unreads"         ?i)
                (list (format "%s AND g:unread AND NOT g:trashed" in-inboxes)             "All unreads"        ?I)
                (list "g:flagged AND NOT g:trashed"                                       "Flagged"            ?f)
                (list "d:today..now"                                                      "Today's"            ?t)
                (list "d:7d..now"                                                         "Last 7 days"        ?w)
                (list "g:attach"                                                          "With attachment(s)" ?a))
          mu4e-maildir-shortcuts
          (list (cons (sheda-communication/mu4e-archive-maildir) ?a)
                (cons (format "%s/bugs"    base)                 ?b)
                (cons (format "%s/drafts"  base)                 ?d)
                (cons (format "%s/list/freebsd" base)            ?f)
                (cons (format "%s/inbox"   base)                 ?i)
                (cons (format "%s/notifications" base)           ?n)
                (cons (format "%s/sent"    base)                 ?s)
                (cons (format "%s/trash"   base)                 ?t))
          mu4e-sent-folder   (format "%s/sent"     base)
          mu4e-trash-folder  (format "%s/trash"    base)
          mu4e-drafts-folder (format "%s/drafts"   base)))

  ;; XXX
  ;; (setq message-send-hook      nil
  ;;       mu4e-compose-mode-hook nil
  ;;       )

  (setq mu4e-debug                 t
        mu4e-compose-format-flowed t
        mu4e-update-interval       120
        mu4e-use-fancy-chars       t
        mu4e-hide-index-messages   t
        mu4e-view-show-addresses   t
        mu4e-get-mail-command      "fetch-mails"
        mu4e-attachment-dir        "~/t/"
        mu4e-refile-folder         'sheda-communication/mu4e-refile-folder-func
        mu4e-headers-results-limit 200
        mm-attachment-override-types '(
                                       "text/x-vcard"
                                       "application/pkcs7-mime"
                                       "application/x-pkcs7-mime"
                                       "application/pkcs7-signature"
                                       "application/x-pkcs7-signature"
                                       "image/.*"
                                       )
        mm-decrypt-option 'always
        mm-verify-option  'always
        mu4e-contact-rewrite-function 'sheda-communication/mu4e-rewrite-function
        mu4e-marks
        '((refile
           :char ("r" . "▶")
           :prompt "refile"
           :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
           :action (lambda (docid msg target) (mu4e~proc-move docid
                                                              (mu4e~mark-check-target target) "-N")))
          (delete
           ;; :char ("D" . "❌")
           :char ("D" . "X")
           :prompt "Delete"
           :show-target (lambda (target) "delete")
           :action (lambda (docid msg target) (mu4e~proc-remove docid)))
          (flag
           :char ("+" . "✚")
           :prompt "+flag"
           :show-target (lambda (target) "flag")
           :action (lambda (docid msg target) (mu4e~proc-move docid nil "+F-u-N")))
          (move
           :char ("m" . "▷")
           :prompt "move"
           :ask-target  mu4e~mark-get-move-target
           :action (lambda (docid msg target) (mu4e~proc-move docid
                                                              (mu4e~mark-check-target target) "-N")))
          (read
           :char    ("!" . "◼")
           :prompt "!read"
           :show-target (lambda (target) "read")
           :action (lambda (docid msg target) (mu4e~proc-move docid nil "+S-u-N")))
          (trash
           :char ("d" . "▼")
           :prompt "dtrash"
           :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
           :action (lambda (docid msg target) (mu4e~proc-move docid
                                                              (mu4e~mark-check-target target) "+T-N")))
          (unflag
           ;; :char    ("-" . "➖")
           :char    ("-" . "-")
           :prompt "-unflag"
           :show-target (lambda (target) "unflag")
           :action (lambda (docid msg target) (mu4e~proc-move docid nil "-F-N")))
          (untrash
           :char   ("=" . "▲")
           :prompt "=untrash"
           :show-target (lambda (target) "untrash")
           :action (lambda (docid msg target) (mu4e~proc-move docid nil "-T")))
          (unread
           :char    ("?" . "◻")
           :prompt "?unread"
           :show-target (lambda (target) "unread")
           :action (lambda (docid msg target) (mu4e~proc-move docid nil "-S+u-N")))
          (unmark
           :char  " "
           :prompt "unmark"
           :action (mu4e-error "No action for unmarking"))
          (action
           :char ( "a" . "◯")
           :prompt "action"
           :ask-target  (lambda () (mu4e-read-option "Action: " mu4e-headers-actions))
           :action  (lambda (docid msg actionfunc)
                      (save-excursion
                        (when (mu4e~headers-goto-docid docid)
                          (mu4e-headers-action actionfunc)))))
          (something
           :char  ("*" . "✱")
           :prompt "*something"
           :action (mu4e-error "No action for deferred mark")))
        ))

(defun sheda-communication/post-init-mu4e ()
  "Post-initialize the mu4e package."

  ;;main
  (add-hook 'mu4e-main-mode-hook    #'sheda-communication/adjust-mu4e-main-mode-map)
  (add-hook 'mu4e-main-mode-hook    #'sheda-communication/add-mu4e-buffer-to-persp-and-switch) ;; XXX Require perp-mode.

  ;; XXX Playing with mu4e-compose-keep-self-cc.
  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (add-to-list 'mu4e-user-mail-address-list user-mail-address)))

  ;; headers
  (add-hook 'mu4e-headers-mode-hook #'sheda-communication/adjust-mu4e-headers-mode-map)
  (add-hook 'mu4e-headers-mode-hook #'sheda-communication/add-mu4e-custom-headers-markers)
  (add-hook 'mu4e-headers-mode-hook #'sheda-communication/add-mu4e-buffer-to-persp-and-switch) ;; XXX Require perp-mode.
  ;; view
  (add-hook 'mu4e-view-mode-hook    #'sheda-communication/adjust-mu4e-view-mode-map)
  (add-hook 'mu4e-view-mode-hook    #'sheda-communication/add-mu4e-buffer-to-persp-and-switch) ;; XXX Require perp-mode.
  ;; compose
  (add-hook 'mu4e-compose-mode-hook #'sheda-communication/adjust-mu4e-compose-mode-map)
  (add-hook 'mu4e-compose-mode-hook #'sheda-communication/add-mu4e-buffer-to-persp-and-switch) ;; XXX Require perp-mode.
  (add-hook 'mu4e-compose-mode-hook (lambda ()
                                      (set-fill-column 66)                                     ;; Keep consistent with fill-flowed-encode-column.
                                      (spacemacs/toggle-fill-column-indicator-on)
                                      (spacemacs/toggle-auto-fill-mode-on)
                                      (flyspell-mode)
                                      ;; (use-hard-newlines -1)
                                      )
            t ;; Append
            )

  ;; Hooks when used in conjunction with persp-mode:
  ;; (eval-after-load "persp-mode"
  ;;   (progn
  ;;     (add-hook 'mu4e-compose-mode-hook #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)
  ;;     (add-hook 'mu4e-headers-mode-hook #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)
  ;;     (add-hook 'mu4e-main-mode-hook    #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)
  ;;     (add-hook 'mu4e-view-mode-hook    #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)))
  )

(defun sheda-communication/post-init-mu4e-alert ()
  "Post-initialize the mu4e-alert package."
  (eval-after-load 'alert #'sheda-communication/setup-alert-style-for-mu4e)
  (let* ((base (sheda-communication/mu4e-base-maildir)))
    (setq mu4e-alert-group-by               :from
          mu4e-alert-interesting-mail-query (format "( m:%s/inbox OR m:%s/irp ) AND g:unread AND NOT g:trashed" base base)
          mu4e-alert-set-window-urgency     nil
          )))

(defun sheda-communication/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@mu4e"
    :binding "m"
    :body
    (call-interactively 'mu4e)))
