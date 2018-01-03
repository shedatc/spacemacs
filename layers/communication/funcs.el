;; (defun sheda/switch-to-mail-buffer-other-window ()
;;   "Jump to the Gnus' *Group* buffer."
;;   (interactive)
;;   (switch-to-buffer-other-window "*Group*"))

;; (defun sheda/jabber-update-activity-count ()
;;   (call-process-shell-command
;;    (format "echo \"%s\" > ~/var/tmp/jabber-activity-count" jabber-activity-count-string)))

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

(defun sheda-communication/mu4e-refile-folder-func (msg)
  "Select the folder according to MSG's From: and To: headers."
  (let ((from (cadr (mu4e-message-field msg :from)))
        (to   (cadr (mu4e-message-field msg :to))))
    (cond
     ((or (s-starts-with-p from "dp")    (s-starts-with-p to "dp"))    "/irp")
     ((or (s-starts-with-p from "ce")    (s-starts-with-p to "ce"))    "/irp")
     ((or (s-starts-with-p from "chsct") (s-starts-with-p to "chsct")) "/irp")
     (t "/archive"))))

(defun sheda-communication/jump-to-containing-maildir (msg)
  "Jump to the maildir containing MSG."
  (let ((maildir (mu4e-message-field msg :maildir)))
    (mu4e~headers-jump-to-maildir maildir)))
