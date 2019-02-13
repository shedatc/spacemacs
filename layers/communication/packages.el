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
    jabber
    (jabber-otr :excluded t)
    mu4e
    mu4e-alert
    persp-mode
    )
)

(defun sheda-communication/init-helm-mu ()
  "Initialize the helm-mu package if the mu4e one is installed."
  (use-package helm-mu
    :if (configuration-layer/package-usedp 'mu4e)
    ;; :commands (list 'helm-mu 'helm-mu-contacts)
    :commands 'helm-mu
    :init
    ;; The "om" prefix is declared by sheda-communication/pre-init-mu4e.
    (spacemacs/set-leader-keys
      "omb"  'mu4e-headers-search-bookmark
      "omj"  'mu4e~headers-jump-to-maildir
      "omm"  'helm-mu)
    (spacemacs/set-leader-keys
      "oC"  'helm-mu-contacts)
    :config
    (setq helm-mu-default-search-string "(m:/inbox OR m:/sent OR m:/irp) AND d:2w..now")
    (helm-add-action-to-source "Jump to containing maildir" 'sheda-communication/jump-to-containing-maildir helm-source-mu)))

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

  ;; Decide on the maildir using the hostname (system-name).
  (setq mu4e-maildir (expand-file-name (concat ".mails/"
                                               (cond ((string= system-name "azathoth.stephaner.labo.int") "stormshield")
                                                     (t                                                   user-login-name)))
                                       user-home-directory))

  (let* ((me (cond ((string= system-name "azathoth.stephaner.labo.int") "stephane.rochoy")
                   (t                                                   user-login-name))))
    (setq mu4e-bookmarks
          (list (list (concat "( m:/inbox OR m:/irp ) AND g:unread AND NOT g:trashed AND t:" me) "My unreads"         ?i)
                (list "( m:/inbox OR m:/irp ) AND g:unread AND NOT g:trashed"                    "All unreads"        ?I)
                (list "g:flagged AND NOT g:trashed"                                              "Flagged"            ?f)
                (list "d:today..now"                                                             "Today's"            ?t)
                (list "d:7d..now"                                                                "Last 7 days"        ?w)
                (list "g:attach"                                                                 "With attachment(s)" ?a))))

  (setq mu4e-maildir-shortcuts
        (list (cons (sheda-communication/mu4e-archive-maildir) ?a)
              (cons "/bugs"                 ?b)
              (cons "/drafts"               ?d)
              (cons "/inbox"                ?i)
              (cons "/irp"                  ?I)
              (cons "/reviews"              ?r)
              (cons "/sent"                 ?s)
              (cons "/trash"                ?t)))

  (setq mu4e-debug               nil
        mu4e-update-interval     120
        mu4e-use-fancy-chars     t
        mu4e-hide-index-messages t
        mu4e-view-show-addresses t
        mu4e-get-mail-command    "~/bin/fetch-mails"
        mu4e-attachment-dir      "~/dl/"
        mu4e-refile-folder       'sheda-communication/mu4e-refile-folder-func

        mu4e-headers-results-limit 100

        mm-attachment-override-types
           '("text/x-vcard" "application/pkcs7-mime" "application/x-pkcs7-mime" "application/pkcs7-signature" "application/x-pkcs7-signature" "image/.*")
        mm-decrypt-option 'always
        mm-verify-option  'always

        mu4e-marks '((refile
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

        ;; XXX Contexts don't seems to work as expected. And don't support
        ;;     changing the maildir :/

        ;; mu4e-context-policy         'ask-if-none
        ;; mu4e-compose-context-policy 'ask-if-none
        ;; mu4e-contexts `( ,(make-mu4e-context
        ;; 				   :name "Sheda"
        ;; 				   :enter-func (lambda () (mu4e-message "Entering Sheda context"))
        ;; 				   :leave-func (lambda () (mu4e-message "Leaving Sheda context"))
        ;; 				   :match-func (lambda (msg) (string= system-name "davinel.mg.rsph.local"))
        ;; 				   :vars '( ( user-mail-address  . "sheda@fsfe.org"  )
        ;; 							( user-full-name     . "Sheda" )
        ;; 							( mu4e-sent-folder   . "/sent" )
        ;; 							( mu4e-trash-folder  . "/trash" )
        ;; 							( mu4e-drafts-folder . "/drafts" )
        ;; 							( mu4e-refile-folder . "/archives" )
        ;; 							))
        ;; 				 ,(make-mu4e-context
        ;; 				   :name "Work"
        ;; 				   :enter-func (lambda () (mu4e-message "Entering Work context"))
        ;; 				   :leave-func (lambda () (mu4e-message "Leaving Work context"))
        ;; 				   :match-func (lambda (msg) (string= system-name "azathoth.stephaner.labo.int"))
        ;; 				   :vars '( ( user-mail-address  . "stephane.rochoy@stormshield.eu"  )
        ;; 							( user-full-name     . "Stéphane Rochoy" )
        ;; 							( mu4e-sent-folder   . "/stormshield/sent" )
        ;; 							( mu4e-trash-folder  . "/stormshield/trash" )
        ;; 							( mu4e-drafts-folder . "/stormshield/drafts" )
        ;; 							( mu4e-refile-folder . "/stormshield/archives" )
        ;; 							)))
        ))

(defun sheda-communication/post-init-mu4e ()
  "Post-initialize the mu4e package."

  ;;main
  (add-hook 'mu4e-main-mode-hook    #'sheda-communication/adjust-mu4e-main-mode-map)
  ;; headers
  (add-hook 'mu4e-headers-mode-hook #'sheda-communication/adjust-mu4e-headers-mode-map)
  (add-hook 'mu4e-headers-mode-hook #'sheda-communication/add-mu4e-custom-headers-markers)
  ;; view
  (add-hook 'mu4e-view-mode-hook    #'sheda-communication/adjust-mu4e-view-mode-map)
  ;; compose
  (add-hook 'mu4e-compose-mode-hook #'sheda-communication/adjust-mu4e-compose-mode-map)
  )

(defun sheda-communication/post-init-mu4e-alert ()
  "Post-initialize the mu4e-alert package."
  (eval-after-load 'alert #'sheda-communication/setup-alert-style-for-mu4e)
  (setq mu4e-alert-group-by               :from
        mu4e-alert-interesting-mail-query "( m:/inbox OR m:/irp ) AND g:unread AND NOT g:trashed AND NOT ( f:mantis OR s:\"Review Request\" )"
        ))

(defun sheda-communication/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@mu4e"
    :binding "m"
    :body
    (progn
      (add-hook 'mu4e-compose-mode-hook #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)
      (add-hook 'mu4e-headers-mode-hook #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)
      (add-hook 'mu4e-main-mode-hook    #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)
      (add-hook 'mu4e-view-mode-hook    #'sheda-communication/add-mu4e-buffer-to-persp-and-switch)
      (call-interactively 'mu4e))
    ;; (let ((agenda-files (org-agenda-files)))
    ;;   (if agenda-files
    ;;       (find-file (first agenda-files))
    ;;     (user-error "Error: No agenda files configured, nothing to display.")))
    ))
