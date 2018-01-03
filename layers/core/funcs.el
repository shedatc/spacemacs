(defun sheda-core/message (format-string &rest args)
  "Display a message but clearly identify it as coming from one of Sheda's layers."
  (message (concat "(sheda) " format-string) args))


(defun sheda-core/switch-to-messages-buffer ()
  "Switch to the *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun sheda-core/switch-to-reminders-buffer ()
  "Switch to the reminders buffer."
  (interactive)
  (let* ((buffer (get-buffer "dot.reminders")))
    (if (null buffer)
        (find-file "~/ens/conf/dot.reminders")
      (switch-to-buffer buffer))))

(defun sheda-core/switch-to-work-org-buffer ()
  "Switch to the work.org buffer."
  (interactive)
  (let* ((buffer (get-buffer "work.org")))
    (if (null buffer)
        (find-file "~/ens/org/work.org")
      (switch-to-buffer buffer))))

(defun sheda-core/run-push ()
  "Run the push command."
  (start-process "push" "*Push Log*" "/home/stephaner/bin/push"))

(defun sheda-core/go-away ()
  "Go away from keyboard."
  (interactive)
  (sheda-core/message "Locking session...")
  ;; XXX Should test for existence before using the jabber-* functions.
  (jabber-send-presence "away" "I'm away from keyboard right now" 0)
  (async-start-process "lock-session"
                       "/home/stephaner/bin/lock-session"
                       (lambda (process)
                         (jabber-send-presence "" "Ready to chat" 0)
                         (sheda-core/message "Session unlocked."))))

(defun sheda-core/adjust-keys-for-helm (map)
  (define-key map (kbd "C-c") 'helm-previous-source)
  (define-key map (kbd "C-t") 'helm-next-line)
  (define-key map (kbd "C-s") 'helm-previous-line)
  (define-key map (kbd "C-r") 'helm-next-source))
