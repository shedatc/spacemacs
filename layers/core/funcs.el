(defun sheda-core/message (format-string &rest args)
  "Display a message but clearly identify it as coming from one of Sheda's layers."
  (message (concat "(sheda) " format-string) args))

(defun sheda-core/switch-to-file-buffer (basename dirname)
  "Switch to the buffer visiting the file BASENAME located in the directory DIRNAME, i.e., DIRNAME/BASENAME."
  (let* ((buffer (get-buffer basename)))
    (if (null buffer)
        (find-file (concat dirname "/" basename))
      (switch-to-buffer buffer))))

(defun sheda-core/run-push ()
  "Run the push command."
  (interactive)
  (start-process "push" "*Push Log*" (expand-file-name "bin/push" user-home-directory)))

(defun sheda-core/adjust-keys-for-helm (map)
  (define-key map (kbd "C-c") 'helm-previous-source)
  (define-key map (kbd "C-t") 'helm-next-line)
  (define-key map (kbd "C-s") 'helm-previous-line)
  (define-key map (kbd "C-r") 'helm-next-source))

;; SWITCHING TO BUFFERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sheda-core/switch-to-messages-buffer ()
  "Switch to the *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))


(defun sheda-core/switch-to-reminders-buffer ()
  "Switch to the reminders buffer."
  (interactive)
  (sheda-core/switch-to-file-buffer "dot.reminders" "~/ens/conf"))

(defun sheda-core/shell-command-on-region (start end command)
  "Passes the content of the region as the standard input to the shell command and replace it with the resulting output."
  (interactive "r\nsCommand: ")
  (shell-command-on-region start end command nil t))

(defun sheda-core/make-backup-file-name (file)
  "Function for `make-backup-file-name'.
Normally this just returns FILE's name with `.orig' appended.
It searches for a match for FILE in `backup-directory-alist'.
If the directory for the backup doesn't exist, it is created.

This function should mirror make-backup-file-name--default-function."
  (concat (make-backup-file-name-1 file) ".orig"))
